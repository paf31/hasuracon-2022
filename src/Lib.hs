{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Lib
  ( main
  ) where
  
import Burrito qualified
import Burrito.Internal.Render qualified 
import Burrito.Internal.Type.Expression qualified as Burrito.Expression
import Burrito.Internal.Type.Name qualified as Burrito.Name
import Burrito.Internal.Type.Template qualified as Burrito.Template
import Burrito.Internal.Type.Token qualified as Burrito.Token
import Burrito.Internal.Type.Value qualified as Burrito.Value
import Burrito.Internal.Type.Variable qualified as Burrito.Variable
import Control.Lens ((^.), (^..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Array, _Object)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml
import Dovetail
import Dovetail.Aeson qualified as JSON 
import Dovetail.Evaluate qualified as Evaluate 
import Dovetail.FFI.Internal qualified as FFI
import Dovetail.Prelude (stdlib)
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API hiding (limit, offset)
import Hasura.Backends.DataWrapper.API.V0.Query qualified as Query
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.CST qualified as CST
import Language.PureScript.Label qualified as Label
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wreq qualified as Wreq
import Servant (Server)
import Servant.API
import Servant.Server (Handler, err500, errBody, serve)
import System.Environment (getArgs)
import System.Exit (die)

data Config = Config
  { source :: FilePath
  , imports :: HashMap Text URLImport
  , engineUrl :: Text
  , tables :: [TableImport]
  } deriving (Show, Generic)
    deriving anyclass Aeson.FromJSON

data URLImport = URLImport
  { uri :: String
  , response :: Text
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data TableImport = TableImport
  { name :: Text
  , columns :: [ColumnImport]
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data ColumnImport = ColumnImport
  { name :: Text
  , dataType :: ScalarType.Type
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data ParsedURLImport = ParsedURLImport Burrito.Template P.SourceType

parseType :: Text -> Either String P.SourceType
parseType input = do
  let tokens = CST.lex input
      (_, parseResult) = CST.runParser (CST.ParserState tokens [] []) CST.parseType
  either (Left . foldMap CST.prettyPrintError) (Right . CST.convertType "<input>") parseResult

parseURLImport :: URLImport -> Either String ParsedURLImport
parseURLImport URLImport{..} = 
  ParsedURLImport
    <$> maybe (Left "unable to parse URI template") Right (Burrito.parse uri)
    <*> parseType response

importToFFI :: (Text, ParsedURLImport) -> Eval () (ForeignImport ())
importToFFI (name, ParsedURLImport url ty) = 
    JSON.reify ty \(_ :: Proxy ty) ->
      pure ForeignImport
        { fv_name = P.Ident name
        , fv_type = args `FFI.function` ty
        , fv_value = toValue @() @(HashMap Text Text -> Eval () ty) \vals -> do
            let uri = Burrito.expand [(Text.unpack k, Burrito.stringValue (Text.unpack v)) | (k, v) <- HashMap.toList vals] url
            res <- liftIO $ Wreq.asJSON @_ @ty =<< Wreq.get uri
            pure (res ^. Wreq.responseBody)
        }
  where
    args = P.TypeApp P.nullSourceAnn P.tyRecord argsRow
    argsRow = P.rowFromList ([P.srcRowListItem var P.tyString | var <- uriVars url], P.srcREmpty)

uriVars :: Burrito.Template -> [Label.Label]
uriVars = foldMap go . Burrito.Template.tokens where
  go :: Burrito.Token.Token -> [Label.Label]
  go (Burrito.Token.Expression o) = foldMap ((:[]) . toLabel . Burrito.Variable.name) (Burrito.Expression.variables o)
  go Burrito.Token.Literal{} = mempty 
  
  toLabel name = Label.Label (PSString.mkString (Text.pack (Burrito.Internal.Render.builderToString (Burrito.Internal.Render.name name))))

api :: Proxy Api
api = Proxy

checkTypeOfMain 
  :: P.SourceType
  -> (forall i o. JSON.Serializable ctx o => Proxy o -> [P.RowListItem P.SourceAnn] -> Eval ctx r)
  -> Eval ctx r
checkTypeOfMain ty f 
  | P.TypeApp _ (P.TypeApp _ fn _) ty1 <- ty
  , fn == P.tyFunction
  , P.TypeApp _ arr ty2 <- ty1
  , arr == P.tyArray
  , P.TypeApp _ rec_ ty3 <- ty2
  , rec_ == P.tyRecord
  = JSON.reify ty2 \proxy ->
      let (knownFields, _) = P.rowToSortedList ty3
       in f proxy knownFields
checkTypeOfMain (P.ForAll _ _ _ ty _) f =
  checkTypeOfMain ty f
checkTypeOfMain _ _ =
  throwErrorWithContext (Evaluate.OtherError "main must have type {} -> Array (Record r)")

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

main :: IO ()
main = do
  [configFile] <- getArgs
  Config{..} <- Yaml.decodeFileThrow configFile
  moduleText <- Text.readFile source
  
  parsedImports <-
    case traverse parseURLImport imports of
      Left err -> die err
      Right x -> pure x

  runInterpretWithDebugger () do
    traverse_ ffi stdlib
    
    ffiDecls <- liftEval $ traverse importToFFI (HashMap.toList parsedImports)
    ffi (FFI (ModuleName "Imports") ffiDecls)
  
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    -- (query, ty) <- eval (Just moduleName) "main"
    -- TODO: parse main as a record with one config key per defined table
    
    -- TODO: add FFI declaration for each table that we can query, including
    -- where clauses and pk lookups
    do
      -- liftEval $ checkTypeOfMain ty \(_ :: Proxy ty) _ -> do
      let server :: Server Api
          server = getSchema :<|> runQuery
          
          getSchema :: Handler SchemaResponse
          getSchema = pure (SchemaResponse caps (map toTableInfo tables))
          
          fromPSType :: P.SourceType -> Type
          fromPSType ty 
            | ty == P.tyInt = NumberTy
            | ty == P.tyNumber = NumberTy
            | ty == P.tyString = StringTy
            | ty == P.tyBoolean = BoolTy
            | otherwise = error $ "bad field type: " <> show ty
          
          toTableInfo :: TableImport -> TableInfo
          toTableInfo tbl = TableInfo
            { dtiName        = TableName (name (tbl :: TableImport))
            , dtiColumns     = [ ColumnInfo 
                                 { dciName = ColumnName (name (col :: ColumnImport)) -- (maybe (error "bad field name") id (PSString.decodeString fieldName))
                                 , dciType = dataType col --fromPSType fieldType
                                 , dciNullable = False
                                 , dciDescription = Nothing
                                 }
                               -- | P.RowListItem _ (Label.Label fieldName) fieldType <- fields 
                               | col <- columns tbl
                               ]
            , dtiPrimaryKey  = Nothing
            , dtiDescription = Nothing
            }
          
          caps :: Capabilities
          caps = Capabilities 
            { dcRelationships = False 
            }
            
          standardQuery :: Text -> [Text] -> Text
          standardQuery tbl cols = 
            "query ($limit: Int, $offset: Int, $where: " <> tbl <> "_bool_exp, $order_by: [" <> tbl <> "_order_by!]) {\
            \  " <> tbl <> "(limit: $limit, offset: $offset, order_by: $order_by, where: $where) {\
            \    " <> Text.unwords cols <> "\
            \  }\
            \}"
          
          runQuery :: Query -> Handler QueryResponse
          runQuery q = do
            let graphqlUrl = engineUrl <> "/v1/graphql"
                TableName tbl = from q
                body = Aeson.object
                         [ "query"     .= standardQuery tbl (HashMap.keys (fields q))
                         , "variables" .= vars
                         ]
                vars = Aeson.object
                         [ "limit"    .= Query.limit q
                         , "offset"   .= Query.offset q
                         
                         -- TODO : encode and send these
                         -- , "where"    .= _
                         -- , "order_by" .= _
                         ]
                         
            res <- liftIO $ Wreq.asJSON @_ @Aeson.Value =<< Wreq.post (Text.unpack graphqlUrl) body
            -- pure (res ^. Wreq.responseBody)
            -- let req = QueryRequest 
            --       { limit = WrappedMaybe (fmap fromIntegral (Query.limit q))
            --       , offset = WrappedMaybe (fmap fromIntegral (Query.offset q))
            --       }
            -- response <- liftIO . runEval () $ fromValueRHS @() @(QueryRequest -> Eval () (Vector ty)) query req
            -- case response of
            --   Left err -> do
            --     -- todo : print error
            --     throwError $ err500 { errBody = "Unexpected error during evaluation, see logs." }
            --   Right rows -> do
            -- let getObject :: Aeson.Value -> Handler Aeson.Object
            --     getObject (Aeson.Object o) = pure o
            --     getObject _ = throwError $ err500 { errBody = "Unexpected error during evaluation, see logs." }
            -- QueryResponse <$> getObject (res ^. Wreq.responseBody)
            
            pure (QueryResponse (res ^.. Wreq.responseBody . key "data" . key tbl . _Array . traverse . _Object))
          
          app :: Application
          app = serve api server
      
      liftIO do
        withStdoutLogger $ \aplogger -> do
          let settings = setPort 8081 $ setLogger aplogger defaultSettings
          putStrLn "Listening on port 8081"
          runSettings settings app