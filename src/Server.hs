module Server
  ( main
  ) where

import Config qualified
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Proxy
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.Prelude (stdlib)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.API.V0.Column qualified as Column
import Hasura.Backends.DataWrapper.API.V0.Table qualified as Table
import HTTP qualified
import HasuraClient qualified
import Language.PureScript.CoreFn qualified as CoreFn
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Program qualified
import Servant (Server)
import Servant.API
import System.Posix.Signals qualified as Signal
import Servant.Server (Handler, serve)
import System.Environment (getArgs)
-- import System.Exit (die)
import Translate qualified


import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Language.PureScript qualified as P
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString
import Language.PureScript.Label qualified as Label
import Dovetail.FFI.Internal qualified as FFI
import Dovetail.FFI.Internal qualified as FFI
import Dovetail.FFI.Internal qualified as FFI
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType

api :: Proxy API.Api
api = Proxy

mkServer :: IORef (HashMap Text Program.TableConfig) -> Config.Config -> Server API.Api
mkServer tableConfigRef Config.Config{..} = getSchema :<|> runQuery where
  getSchema :: Handler API.SchemaResponse
  getSchema = pure (API.SchemaResponse capabilities (map toTableInfo (HashMap.toList tables))) where
    toTableInfo :: (Text, Config.TableImport) -> Table.TableInfo
    toTableInfo (name, tbl) = Table.TableInfo
      { dtiName        = Table.TableName name
      , dtiColumns     = [ Column.ColumnInfo 
                           { dciName = Column.ColumnName (Config.name (col :: Config.ColumnImport)) -- (maybe (error "bad field name") id (PSString.decodeString fieldName))
                           , dciType = Config.dataType col --fromPSType fieldType
                           , dciNullable = False
                           , dciDescription = Nothing
                           }
                         -- | P.RowListItem _ (Label.Label fieldName) fieldType <- fields 
                         | col <- Config.columns tbl
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
      Just (Program.TableConfig (Evaluate.ForeignType predicate)) -> do
        let newPredicate = 
              case HasuraClient.where_ hasuraQuery of
                Nothing -> 
                  Just predicate
                Just predicate' -> 
                  Just $ HasuraClient.And [predicate, predicate']
            newQuery = hasuraQuery { HasuraClient.where_ = newPredicate }
        res <- liftIO $ HasuraClient.runQuery engineUrl newQuery
        pure (API.QueryResponse res)
      Nothing -> do
        res <- liftIO $ HasuraClient.runQuery engineUrl hasuraQuery
        pure (API.QueryResponse res)
        
loadConfig
  :: Config.Config
  -> Eval () [ForeignImport ()]
  -> IO (Either (InterpretError ()) (HashMap Text Program.TableConfig))
loadConfig config httpImports = do
  moduleText <- Text.readFile (Config.source config)
  runInterpret () do
    traverse_ ffi stdlib
    
    ffiDecls <- liftEval httpImports
    ffi (FFI (ModuleName "Imports") ffiDecls)
    
    ffi (FFI (ModuleName "Client") (map (tableToImport (Config.engineUrl config)) (HashMap.toList (Config.tables config))))
    
    _ <- Program.install (Config.tables config)
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    Program.evalConfig config moduleName

-- newtype WrappedMaybe a = WrappedMaybe (Maybe a)
--   deriving stock (Show, Eq, Generic)
-- 
-- instance ToValue ctx a => ToValue ctx (WrappedMaybe a) where
--   toValue (WrappedMaybe Nothing) = 
--     Evaluate.Constructor (Names.ProperName "Nothing") []
--   toValue (WrappedMaybe (Just a)) = 
--     Evaluate.Constructor (Names.ProperName "Just") [toValue a]
-- 
--   fromValue (Evaluate.Constructor (Names.ProperName "Nothing") []) =
--     pure (WrappedMaybe Nothing)
--   fromValue (Evaluate.Constructor (Names.ProperName "Just") [val]) =
--     WrappedMaybe . Just <$> fromValue val
--   fromValue other =
--     Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "Maybe" other)

data FFIQuery = FFIQuery
  { where_ :: Evaluate.ForeignType HasuraClient.Predicate
  , limit :: Integer
  , offset :: Integer
  } 
  deriving stock (Generic)
  deriving anyclass (Evaluate.ToValue ctx)

tableToImport :: Text -> (Text, Config.TableImport) -> ForeignImport ()
tableToImport engineUrl (name, Config.TableImport columns) = 
    ForeignImport
      { fv_name = P.Ident name
      , fv_type = (cols `FFI.function` args) `FFI.function` FFI.array result
      , fv_value = toValue @() @((HashMap Text Text -> Eval () FFIQuery) -> Eval () (Vector (HashMap Text (Evaluate.Value ())))) \f -> do
          let columnsRecord = HashMap.fromList [ (col, col) | Config.ColumnImport col _ <- columns ]
          query <- f columnsRecord
          let hasuraQuery = HasuraClient.Query
                { HasuraClient.table = name -- :: Text
                , HasuraClient.fields = [ HasuraClient.Column col | Config.ColumnImport col _ <- columns] -- :: [Column]
                , HasuraClient.where_ = Just (Evaluate.getForeignType (where_ query)) -- :: Maybe Predicate
                , HasuraClient.orderBy = Nothing -- :: Maybe (NonEmpty (Column, OrderDirection))
                , HasuraClient.limit = Just (fromIntegral (limit query)) -- :: Maybe Int
                , HasuraClient.offset = Just (fromIntegral (offset query)) -- :: Maybe Int
                }
          res <- liftIO $ HasuraClient.runQuery engineUrl hasuraQuery
          pure (Vector.fromList (map (fmap toPSValue) res))
      }
  where
    columnType :: ScalarType.Type -> P.SourceType
    columnType ty = P.TypeApp P.nullSourceAnn (P.TypeConstructor P.nullSourceAnn (P.mkQualified (P.ProperName "Column") (P.ModuleName "Supercharger"))) (toPSType ty)
    
    toPSType :: ScalarType.Type -> P.SourceType
    toPSType ScalarType.StringTy = P.tyString
    toPSType ScalarType.NumberTy = P.tyNumber
    toPSType ScalarType.BoolTy = P.tyBoolean
    -- TODO: handle nullable types
    
    toPSValue :: Yaml.Value -> Evaluate.Value ()
    toPSValue (Yaml.String s) = Evaluate.String s
    toPSValue (Yaml.Number d) = Evaluate.Number (realToFrac d)
    toPSValue (Yaml.Bool b) = Evaluate.Bool b
    toPSValue _ = error "unexpected value in gql response"
    
    cols = P.TypeApp P.nullSourceAnn P.tyRecord colsRow
    colsRow = P.rowFromList 
      ( [P.srcRowListItem (Label.Label (PSString.mkString col)) (columnType ty) | Config.ColumnImport col ty <- columns]
      , P.srcREmpty
      )
    
    args = P.TypeApp P.nullSourceAnn P.tyRecord argsRow
    argsRow = P.rowFromList 
      ( [ P.srcRowListItem (Label.Label (PSString.mkString "limit")) P.tyInt
        , P.srcRowListItem (Label.Label (PSString.mkString "offset")) P.tyInt
        , P.srcRowListItem (Label.Label (PSString.mkString "where_"))
            (P.TypeConstructor P.nullSourceAnn (P.mkQualified (P.ProperName "Predicate") (P.ModuleName "Supercharger")))
        ]
      , P.srcREmpty
      )
      -- TODO: add Maybe types
    
    result = P.TypeApp P.nullSourceAnn P.tyRecord resultRow
    resultRow = P.rowFromList 
      ( [P.srcRowListItem (Label.Label (PSString.mkString col)) (toPSType ty) | Config.ColumnImport col ty <- columns]
      , P.srcREmpty
      )

main :: IO ()
main = do
  [configFile] <- getArgs
  config@Config.Config{..} <- Yaml.decodeFileThrow configFile
  
  httpImports <- HTTP.importAll imports

  result <- loadConfig config httpImports
    
  case result of
    Right tableConfigs -> do
      tableConfigRef <- liftIO $ IORef.newIORef tableConfigs
      
      let reloadConfig = do
            newResult <- loadConfig config httpImports
            case newResult of
              Right newTableConfigs -> do
                IORef.writeIORef tableConfigRef newTableConfigs
                putStrLn "Config reloaded"
              Left err -> 
                putStrLn $ renderInterpretError defaultTerminalRenderValueOptions err
          
      _ <- liftIO $ Signal.installHandler Signal.sigHUP (Signal.Catch reloadConfig) Nothing
          
      -- TODO: add FFI declaration for each table that we can query, including
      -- where clauses and pk lookups
      let server = mkServer tableConfigRef config
            
          app :: Application
          app = serve api server
        
      liftIO do
        withStdoutLogger $ \aplogger -> do
          let settings = setPort 8081 $ setLogger aplogger defaultSettings
          putStrLn "Listening on port 8081"
          runSettings settings app
          
    Left err -> 
      putStrLn $ renderInterpretError defaultTerminalRenderValueOptions err