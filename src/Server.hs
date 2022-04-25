module Server
  ( main
  ) where

import Config qualified
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Proxy
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import Dovetail
import Dovetail.Aeson qualified as JSON 
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.Prelude (stdlib)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.API.V0.Column qualified as Column
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
import Servant.Server (Handler, serve)
import System.Environment (getArgs)
import Translate qualified

api :: Proxy API.Api
api = Proxy

mkServer :: Config.Config -> Server API.Api
mkServer Config.Config{..} = getSchema :<|> runQuery where
  getSchema :: Handler API.SchemaResponse
  getSchema = pure (API.SchemaResponse capabilities (map toTableInfo tables)) where
    toTableInfo :: Config.TableImport -> Table.TableInfo
    toTableInfo tbl = Table.TableInfo
      { dtiName        = Table.TableName (Config.name (tbl :: Config.TableImport))
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
  runQuery = 
    fmap API.QueryResponse
    . liftIO
    . HasuraClient.runQuery engineUrl
    . Translate.toHasuraQuery

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
  
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    -- (untypedProgram, ty) <- eval (Just moduleName) "main"
    -- let x = Program.inferTables ty untypedProgram
    -- TODO: parse main as a record with one config key per defined table
    
    -- TODO: add FFI declaration for each table that we can query, including
    -- where clauses and pk lookups
    do
      -- liftEval $ checkTypeOfMain ty \(_ :: Proxy ty) _ -> do
      let server = mkServer config
          
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
