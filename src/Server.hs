module Server
  ( server
  ) where

import Config qualified
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Data.Align (alignWith)
import Data.Proxy
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Dovetail
import Dovetail.Core qualified as Core
import Dovetail.Aeson qualified
import FFI.HTTP qualified as HTTP
import FFI.HGE qualified as HGE
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.API.V0.Column qualified as Column
import Hasura.Backends.DataWrapper.API.V0.Table qualified as Table
import HasuraClient qualified
import Language.PureScript.CoreFn qualified as CoreFn
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Program qualified
import Servant (Server, err500)
import Servant.API
import System.Posix.Signals qualified as Signal
import Servant.Server (Handler, serve)
import Translate qualified
import Utils qualified

api :: Proxy API.Api
api = Proxy

mkServer :: IORef (HashMap Text Program.EvaluatedTableConfig) -> Config.Config -> Server API.Api
mkServer tableConfigRef Config.Config{..} = getSchema :<|> runQuery where
  getSchema :: Handler API.SchemaResponse
  getSchema = do
      tableConfigs <- liftIO $ IORef.readIORef tableConfigRef
      pure (API.SchemaResponse capabilities (map (toTableInfo tableConfigs) (HashMap.toList tables)))     
    where
      toTableInfo :: HashMap Text Program.EvaluatedTableConfig -> (Text, Config.TableImport) -> Table.TableInfo
      toTableInfo tableConfigs (name, tbl) =
        let Program.EvaluatedTableConfig (Program.TableConfigType extraColumns) _ _ = 
              case HashMap.lookup name tableConfigs of
                Just tableType -> tableType
                Nothing -> error "table was not configured"
         in Table.TableInfo
              { dtiName        = Table.TableName name
              , dtiColumns     = [ Column.ColumnInfo 
                                   { dciName = Column.ColumnName columnName
                                   , dciType = dataType
                                   , dciNullable = nullable
                                   , dciDescription = Nothing
                                   }
                                 | Config.ColumnImport columnName dataType nullable <- Config.columns tbl
                                 ] <> 
                                 [ Column.ColumnInfo 
                                   { dciName = Column.ColumnName columnName
                                   , dciType = dataType
                                   , dciNullable = nullable
                                   , dciDescription = Nothing
                                   }
                                 | (columnName, Program.ExtraColumn dataType nullable) <- HashMap.toList extraColumns
                                 ]
              , dtiPrimaryKey  = Nothing
              , dtiDescription = Nothing
              }
      
      capabilities :: API.Capabilities
      capabilities = API.Capabilities 
        { dcRelationships = False 
        }
                
  runQuery :: API.Query -> Handler API.QueryResponse
  runQuery q = do
    tableConfigs <- liftIO $ IORef.readIORef tableConfigRef
    let hasuraQuery = Translate.toHasuraQuery q
    case HashMap.lookup (HasuraClient.table hasuraQuery) tableConfigs of
      Just (Program.EvaluatedTableConfig (Program.TableConfigType extraColumns) predicate mkExtras) -> do
        let extraFields = HashMap.keys extraColumns
            newPredicate = 
              case HasuraClient.where_ hasuraQuery of
                Nothing -> 
                  Just predicate
                Just predicate' -> 
                  Just $ HasuraClient.And [predicate, predicate']
            newQuery = hasuraQuery 
              { HasuraClient.fields = 
                  filter
                    (\(HasuraClient.Column c) -> c `notElem` extraFields) 
                    (HasuraClient.fields hasuraQuery)
              , HasuraClient.where_ = 
                  newPredicate 
              }
        rawData <- liftIO $ HasuraClient.runQuery engineUrl newQuery
        let Just (Config.TableImport columns) = HashMap.lookup (HasuraClient.table hasuraQuery) tables
            requestedFields = [ c | HasuraClient.Column c <- HasuraClient.fields hasuraQuery ]
            columnsByName = HashMap.fromList [ (col, c) 
                                             | c@(Config.ColumnImport col _ _) <- columns
                                             , col `elem` requestedFields
                                             ]
        e <- liftIO $ runEval () do
          rows <- traverse (\row -> do
            vals <- sequence (alignWith Utils.toPSValue columnsByName row)
            -- TODO: only include those extras which were requested
            extras <- mkExtras vals
            let allFields = row <> fmap Utils.fromPSValue extras
            pure (HashMap.filterWithKey (\k _ -> k `elem` requestedFields) allFields)) rawData
          pure (API.QueryResponse rows)
        case e of
          Left err -> do
            -- TODO: better error
            liftIO . putStrLn $ renderEvaluationError defaultTerminalRenderValueOptions err
            throwError err500
          Right res ->
            pure res
      Nothing -> do
        res <- liftIO $ HasuraClient.runQuery engineUrl hasuraQuery
        pure (API.QueryResponse res)
      
loadConfig
  :: Config.Config
  -> Eval () [ForeignImport ()]
  -> IO (Either (InterpretError ()) (HashMap Text Program.EvaluatedTableConfig))
loadConfig config imports = do
  moduleText <- Text.readFile (Config.source config)
  runInterpret () do
    Core.buildModules Core.minimal
    
    ffiDecls <- liftEval imports
    ffi (FFI (ModuleName "Imports") ffiDecls)
    
    _ <- Dovetail.Aeson.stdlib
    
    _ <- Program.install (Config.tables config)
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    Program.evalConfig config moduleName

server :: FilePath -> IO ()
server configFile = do
  config@Config.Config{..} <- Yaml.decodeFileThrow configFile
  
  httpImports <- HTTP.importAll imports
  let hgeImports = HGE.importAll config
  let allImports = (<> hgeImports) <$> httpImports

  result <- loadConfig config allImports
    
  case result of
    Right tableConfigs -> do
      tableConfigRef <- liftIO $ IORef.newIORef tableConfigs
      
      let reloadConfig = do
            newResult <- loadConfig config allImports
            case newResult of
              Right newTableConfigs -> do
                IORef.writeIORef tableConfigRef newTableConfigs
                putStrLn "Config reloaded"
              Left err -> 
                putStrLn $ renderInterpretError defaultTerminalRenderValueOptions err
          
      _ <- liftIO $ Signal.installHandler Signal.sigHUP (Signal.Catch reloadConfig) Nothing
            
      let app :: Application
          app = serve api (mkServer tableConfigRef config)
        
      liftIO do
        withStdoutLogger $ \aplogger -> do
          let settings = setPort 8081 $ setLogger aplogger defaultSettings
          putStrLn "Listening on port 8081"
          runSettings settings app
          
    Left err -> 
      putStrLn $ renderInterpretError defaultTerminalRenderValueOptions err
      