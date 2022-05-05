module Server
  ( main
  ) where

import Config qualified
import Control.Monad.IO.Class (liftIO)
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
import System.Environment (getArgs)
import Translate qualified

import Control.Monad.Error.Class (throwError)
import Data.Align (alignWith)
import Data.These (These(..))
import Dovetail.Evaluate qualified as Evaluate
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import Language.PureScript.Names qualified as Names

api :: Proxy API.Api
api = Proxy

mkServer :: IORef (HashMap Text Program.EvaluatedTableConfig) -> Config.Config -> Server API.Api
mkServer tableConfigRef Config.Config{..} = getSchema :<|> runQuery where
  getSchema :: Handler API.SchemaResponse
  getSchema = pure (API.SchemaResponse capabilities (map toTableInfo (HashMap.toList tables))) where
    toTableInfo :: (Text, Config.TableImport) -> Table.TableInfo
    toTableInfo (name, tbl) = Table.TableInfo
      { dtiName        = Table.TableName name
      , dtiColumns     = [ Column.ColumnInfo 
                           { dciName = Column.ColumnName columnName
                           , dciType = dataType
                           , dciNullable = nullable
                           , dciDescription = Nothing
                           }
                         | Config.ColumnImport columnName dataType nullable <- Config.columns tbl
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
      Just (Program.EvaluatedTableConfig predicate mkExtras) -> do
        let newPredicate = 
              case HasuraClient.where_ hasuraQuery of
                Nothing -> 
                  Just predicate
                Just predicate' -> 
                  Just $ HasuraClient.And [predicate, predicate']
            newQuery = hasuraQuery { HasuraClient.where_ = newPredicate }
        res <- liftIO $ HasuraClient.runQuery engineUrl newQuery
        let fetchExtras :: Yaml.Object -> Handler Yaml.Object
            fetchExtras row = do
              let Just (Config.TableImport columns) = HashMap.lookup (HasuraClient.table hasuraQuery) tables
                  columnsByName = HashMap.fromList [ (col, c) | c@(Config.ColumnImport col _ _) <- columns ]
              e <- liftIO $ Evaluate.runEval () do
                values <- sequence $ alignWith toPSValue columnsByName row
                mkExtras values
              case e of
                Left _err -> do
                  throwError err500
                Right extras ->
                  pure (row <> HashMap.map fromPSValue extras)
        API.QueryResponse <$> traverse fetchExtras res
        -- pure (API.QueryResponse res)
      Nothing -> do
        res <- liftIO $ HasuraClient.runQuery engineUrl hasuraQuery
        pure (API.QueryResponse res)
        
fromPSValue :: Evaluate.Value () -> Yaml.Value
fromPSValue (Evaluate.Constructor (Names.ProperName "Nothing") []) = Yaml.Null
fromPSValue (Evaluate.Constructor (Names.ProperName "Just") [val]) = fromPSValue val
fromPSValue (Evaluate.String s) = Yaml.String s
fromPSValue (Evaluate.Number d) = Yaml.Number (realToFrac d)
fromPSValue (Evaluate.Bool b) = Yaml.Bool b
fromPSValue _other = error "unexpected value in fromPSValue"
        
toPSValue :: These Config.ColumnImport Yaml.Value -> Eval () (Evaluate.Value ())
toPSValue (These (Config.ColumnImport _ _ True) Yaml.Null) = 
  pure (Evaluate.Constructor (Names.ProperName "Nothing") [])
toPSValue (These (Config.ColumnImport _ ty True) val) = 
  (Evaluate.Constructor (Names.ProperName "Just") . (:[])) <$> toNonNullPSValue ty val
toPSValue (These (Config.ColumnImport _ ty False) val) =
  toNonNullPSValue ty val
toPSValue (This (Config.ColumnImport columnName _ _)) =
  Evaluate.throwErrorWithContext (Evaluate.OtherError ("error in gql response: missing field " <> columnName))
toPSValue That{} =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: unexpected fields")
  
toNonNullPSValue ScalarType.StringTy (Yaml.String s) = 
  pure $ Evaluate.String s
toNonNullPSValue ScalarType.StringTy _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: expected string")
toNonNullPSValue ScalarType.NumberTy (Yaml.Number d) = 
  pure $ Evaluate.Number (realToFrac d)
toNonNullPSValue ScalarType.NumberTy _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: expected number")
toNonNullPSValue ScalarType.BoolTy (Yaml.Bool b) = 
  pure $ Evaluate.Bool b
toNonNullPSValue ScalarType.BoolTy _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: expected boolean")
        
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
    
    _ <- Program.install (Config.tables config)
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    Program.evalConfig config moduleName

main :: IO ()
main = do
  [configFile] <- getArgs
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
      
      let server = mkServer tableConfigRef config
            
      let app :: Application
          app = serve api server
        
      liftIO do
        withStdoutLogger $ \aplogger -> do
          let settings = setPort 8081 $ setLogger aplogger defaultSettings
          putStrLn "Listening on port 8081"
          runSettings settings app
          
    Left err -> 
      putStrLn $ renderInterpretError defaultTerminalRenderValueOptions err
      