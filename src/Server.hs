module Server
  ( main
  ) where

import Config qualified
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Proxy
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Dovetail
import Dovetail.Aeson qualified as JSON 
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.FFI (FFI(..), ForeignImport(..))
import Dovetail.FFI.Builder (array, boolean, char, int, string, number, (~>))
import Dovetail.FFI.Builder qualified as FFI
import Dovetail.FFI.Internal (forAll, function)
import Dovetail.Prelude (stdlib)
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.API.V0.Column qualified as Column
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
import Hasura.Backends.DataWrapper.API.V0.Table qualified as Table
import HTTP qualified
import HasuraClient qualified
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Program qualified
import Servant (Server)
import Servant.API
import Servant.Server (Handler, err404, serve)
import System.Environment (getArgs)
import System.Exit (die)
import Translate qualified

api :: Proxy API.Api
api = Proxy

mkServer :: HashMap Text TableConfig -> Config.Config -> Server API.Api
mkServer tableConfigs Config.Config{..} = getSchema :<|> runQuery where
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
    let hasuraQuery = Translate.toHasuraQuery q
    case HashMap.lookup (HasuraClient.table hasuraQuery) tableConfigs of
      Just (TableConfig (Evaluate.ForeignType predicate)) -> do
        let newPredicate = 
              case HasuraClient.where_ hasuraQuery of
                Nothing -> 
                  Just $ HasuraClient.Atom predicate
                Just predicate' -> 
                  Just $ HasuraClient.And [HasuraClient.Atom predicate, predicate']
            newQuery = hasuraQuery { HasuraClient.where_ = newPredicate }
        res <- liftIO $ HasuraClient.runQuery engineUrl newQuery
        pure (API.QueryResponse res)
      Nothing ->
        throwError err404
      

filterModule :: Text
filterModule =
  "module Filter where\n\
  \\n\
  \foreign import data Column :: Type -> Type\n\
  \\n\
  \foreign import data Filter :: Type\n\
  \\n\
  \foreign import eq :: forall a. Column a -> a -> Filter"

data TableConfig = TableConfig
  { predicate :: Evaluate.ForeignType HasuraClient.SimplePredicate
  }
  deriving stock Generic
  deriving anyclass (ToValue ctx)

main :: IO ()
main = do
  [configFile] <- getArgs
  config@Config.Config{..} <- Yaml.decodeFileThrow configFile
  moduleText <- Text.readFile source

  httpImports <- HTTP.importAll imports

  runInterpretWithDebugger () do
    traverse_ ffi stdlib
    
    ffiDecls <- liftEval httpImports
    ffi (FFI (ModuleName "Imports") ffiDecls)

    _ <- build filterModule
    loadEnv do
      let fromValue (Evaluate.String s) = Scalar.String s
          fromValue (Evaluate.Int i) = Scalar.Number (fromIntegral i)
          fromValue (Evaluate.Number d) = Scalar.Number (realToFrac d)
          fromValue (Evaluate.Bool b) = Scalar.Boolean b
      Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.SimplePredicate))
        (P.ModuleName "Filter") "eq" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Eq (HasuraClient.Column col) (fromValue val)))
  
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    -- TODO: check the type matches the config
    (untypedConfig, ty) <- eval @_ @(Eval () (Value ())) (Just moduleName) "main"
    tableConfigs <- liftEval do
      evaluatedConfig <- liftIO $ runEval () untypedConfig
      case evaluatedConfig of
        Right (Evaluate.Object tables) -> do
          let evalTableConfig :: Text -> Value () -> Eval () TableConfig
              evalTableConfig tbl fn =
                case HashMap.lookup tbl (Config.tables config) of
                  Just Config.TableImport { columns } -> do
                    let cols = Evaluate.Object (HashMap.fromList [ (Config.name c, Evaluate.String (Config.name c)) | c <- columns ])
                    fromValue @_ @TableConfig =<< Evaluate.apply fn cols
                  Nothing -> liftIO $ die "table not configured"
          HashMap.traverseWithKey evalTableConfig tables
        _ -> 
          liftIO $ die "Config must be a record of tables"
        
    -- liftIO do
    --   x <- runEval () untypedProgram
    --   case x of 
    --     Right y -> print $ renderValue defaultTerminalRenderValueOptions y
    --   putStrLn (P.prettyPrintType 5 ty)
    -- 
    -- 
    -- let x = Program.inferTables ty untypedProgram
    -- TODO: parse main as a record with one config key per defined table
    
    -- TODO: add FFI declaration for each table that we can query, including
    -- where clauses and pk lookups
    do
      -- liftEval $ checkTypeOfMain ty \(_ :: Proxy ty) _ -> do
      let server = mkServer tableConfigs config
          
          app :: Application
          app = serve api server
      
      liftIO do
        withStdoutLogger $ \aplogger -> do
          let settings = setPort 8081 $ setLogger aplogger defaultSettings
          putStrLn "Listening on port 8081"
          runSettings settings app
          
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
--
-- fromPSType :: P.SourceType -> Type
-- fromPSType ty 
--   | ty == P.tyInt = NumberTy
--   | ty == P.tyNumber = NumberTy
--   | ty == P.tyString = StringTy
--   | ty == P.tyBoolean = BoolTy
--   | otherwise = error $ "bad field type: " <> show ty
