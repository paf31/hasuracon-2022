{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Lib
  ( main
  ) where
  
import Autodocodec.Extended qualified as Codec
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
import Data.List.NonEmpty qualified as NEL
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
import Hasura.Backends.DataWrapper.API.V0.Expression qualified as Expression
import Hasura.Backends.DataWrapper.API.V0.OrderBy qualified as OrderBy
import Hasura.Backends.DataWrapper.API.V0.Query qualified as Query
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
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

pattern And_ xs = (Expression.And (Codec.ValueWrapper xs))
pattern Or_ xs = (Expression.Or (Codec.ValueWrapper xs))
pattern Not_ x = (Expression.Not (Codec.ValueWrapper x))
pattern Literal_ x = (Expression.Literal (Codec.ValueWrapper x))
pattern Equal_ x y = (Expression.Equal (Codec.ValueWrapper2 x y))
pattern NotEqual_ x y = (Expression.NotEqual (Codec.ValueWrapper2 x y))
pattern In_ x y = (Expression.In (Codec.ValueWrapper2 x y))
pattern IsNull_ x = (Expression.IsNull (Codec.ValueWrapper x))
pattern IsNotNull_ x = (Expression.IsNotNull (Codec.ValueWrapper x))
pattern Column_ x = (Expression.Column (Codec.ValueWrapper x))
pattern ApplyOperator_ op x y = (Expression.ApplyOperator (Codec.ValueWrapper3 op x y))

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

          encodeValue :: Scalar.Value -> Aeson.Value
          encodeValue (Scalar.String s) = Aeson.String s
          encodeValue (Scalar.Number s) = Aeson.Number s
          encodeValue (Scalar.Boolean s) = Aeson.Bool s
          encodeValue Scalar.Null = Aeson.Null
          
          encodeOperator :: Expression.Operator -> Text
          encodeOperator LessThan = "_lt"
          encodeOperator LessThanOrEqual = "_lte"
          encodeOperator GreaterThan = "_gt"
          encodeOperator GreaterThanOrEqual = "_gte"
          
          encodeExpression :: Expression.Expression -> Aeson.Value
          encodeExpression (And_ xs)
            = Aeson.object [ "_and" .= map encodeExpression xs ]
          encodeExpression (Or_ xs)
            = Aeson.object [ "_or" .= map encodeExpression xs ]
          encodeExpression (Not_ x)
            = Aeson.object [ "_not" .= encodeExpression x ]
          encodeExpression other = encodeSimpleExpression other
          
          encodeSimpleExpression :: Expression.Expression -> Aeson.Value
          encodeSimpleExpression (Equal_ (Column_ (ColumnName c)) (Literal_ v))
            = Aeson.object [ c .= Aeson.object [ "_eq" .= encodeValue v ] ]
          encodeSimpleExpression (NotEqual_ (Column_ (ColumnName c)) (Literal_ v))
            = Aeson.object [ c .= Aeson.object [ "_neq" .= encodeValue v ] ]
          encodeSimpleExpression (In_ (Column_ (ColumnName c)) vs)
            =  Aeson.object [ "_or" .= [ Aeson.object [ c .= Aeson.object [ "_eq" .= encodeValue v ] ] | v <- vs ] ]
          encodeSimpleExpression (IsNull_ (Column_ (ColumnName c)))
            = Aeson.object [ c .= Aeson.object [ "_is_null" .= True ] ]
          encodeSimpleExpression (IsNotNull_ (Column_ (ColumnName c)))
            = Aeson.object [ c .= Aeson.object [ "_is_null" .= False ] ]
          encodeSimpleExpression (ApplyOperator_ op (Column_ (ColumnName c)) (Literal_ v))
            = Aeson.object [ c .= Aeson.object [ encodeOperator op .= encodeValue v ] ]
          encodeSimpleExpression other
            = error $ "Unsupported expression: " <> show other
          
          encodeOrderBy :: [OrderBy.OrderBy] -> Aeson.Value
          encodeOrderBy = Yaml.array . map one where
            one (OrderBy.OrderBy (ColumnName col) dir) = 
              Aeson.object
                [ col .= case dir of
                           OrderBy.Ascending -> "asc" :: Text
                           OrderBy.Descending -> "desc"
                ]
          
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
                         , "where"    .= maybe Aeson.Null encodeExpression (Query.where_ q)
                         , "order_by" .= maybe Aeson.Null (encodeOrderBy . NEL.toList) (Query.orderBy q)
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