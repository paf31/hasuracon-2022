module HasuraClient where 

import Control.Lens ((^..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Array, _Object)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
-- import Dovetail
-- import Dovetail.Aeson qualified as JSON 
-- import Dovetail.Evaluate qualified as Evaluate
-- import Hasura.Backends.DataWrapper.API q
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
-- import Language.PureScript qualified as P
-- import Language.PureScript.CoreFn qualified as CoreFn
import Network.Wreq qualified as Wreq

newtype Column = Column { getColumn :: Text }
  deriving Show

data Predicate 
  = Atom SimplePredicate 
  | And [Predicate]
  | Or [Predicate]
  | Not Predicate
  deriving Show

data SimplePredicate
  = Eq Column Scalar.Value
  | Neq Column Scalar.Value
  | In Column [Scalar.Value]
  | IsNull Column
  | IsNotNull Column
  | Lt Column Scalar.Value
  | Lte Column Scalar.Value
  | Gt Column Scalar.Value
  | Gte Column Scalar.Value
  deriving Show

data OrderDirection = Asc | Desc
  deriving Show

data Query = Query
  { table :: Text
  , fields :: [Column]
  , where_ :: Maybe Predicate
  , orderBy :: Maybe (NonEmpty (Column, OrderDirection))
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving Show

encodeQuery :: Query -> Aeson.Value
encodeQuery Query{..} =
    Aeson.object
      [ "query"     .= queryText table fields
      , "variables" .= vars
      ]
  where
    vars = Aeson.object
             [ "limit"    .= limit
             , "offset"   .= offset
             , "where"    .= maybe Aeson.Null encodeExpression where_
             , "order_by" .= maybe Aeson.Null (encodeOrderBy . NEL.toList) orderBy
             ]     
             
encodeValue :: Scalar.Value -> Aeson.Value
encodeValue (Scalar.String s) = Aeson.String s
encodeValue (Scalar.Number s) = Aeson.Number s
encodeValue (Scalar.Boolean s) = Aeson.Bool s
encodeValue Scalar.Null = Aeson.Null

encodeExpression :: Predicate -> Aeson.Value
encodeExpression (And xs)
  = Aeson.object [ "_and" .= map encodeExpression xs ]
encodeExpression (Or xs)
  = Aeson.object [ "_or" .= map encodeExpression xs ]
encodeExpression (Not x)
  = Aeson.object [ "_not" .= encodeExpression x ]
encodeExpression (Atom other) 
  = encodeSimpleExpression other

encodeSimpleExpression :: SimplePredicate -> Aeson.Value
encodeSimpleExpression (Eq (Column c) v)
  = Aeson.object [ c .= Aeson.object [ "_eq" .= encodeValue v ] ]
encodeSimpleExpression (Neq (Column c) v)
  = Aeson.object [ c .= Aeson.object [ "_neq" .= encodeValue v ] ]
encodeSimpleExpression (In (Column c) vs)
  = Aeson.object [ "_or" .= [ Aeson.object [ c .= Aeson.object [ "_eq" .= encodeValue v ] ] | v <- vs ] ]
encodeSimpleExpression (IsNull (Column c))
  = Aeson.object [ c .= Aeson.object [ "_is_null" .= True ] ]
encodeSimpleExpression (IsNotNull (Column c))
  = Aeson.object [ c .= Aeson.object [ "_is_null" .= False ] ]
encodeSimpleExpression (Lt (Column c) v)
  = Aeson.object [ c .= Aeson.object [ "_lt" .= encodeValue v ] ]
encodeSimpleExpression (Gt (Column c) v)
  = Aeson.object [ c .= Aeson.object [ "_gt" .= encodeValue v ] ]
encodeSimpleExpression (Lte (Column c) v)
  = Aeson.object [ c .= Aeson.object [ "_lte" .= encodeValue v ] ]
encodeSimpleExpression (Gte (Column c) v)
  = Aeson.object [ c .= Aeson.object [ "_gte" .= encodeValue v ] ]

encodeOrderBy :: [(Column, OrderDirection)] -> Aeson.Value
encodeOrderBy = Yaml.array . map one where
  one (Column col, dir) = 
    Aeson.object
      [ col .= case dir of
                 Asc -> "asc" :: Text
                 Desc -> "desc"
      ]
      
queryText :: Text -> [Column] -> Text
queryText tbl cols = 
  "query ($limit: Int, $offset: Int, $where: " <> tbl <> "_bool_exp, $order_by: [" <> tbl <> "_order_by!]) {\
  \  " <> tbl <> "(limit: $limit, offset: $offset, order_by: $order_by, where: $where) {\
  \    " <> Text.unwords (map getColumn cols) <> "\
  \  }\
  \}"
    
runQuery :: Text -> Query -> IO [Aeson.Object]
runQuery engineUrl q = do
  let graphqlUrl = engineUrl <> "/v1/graphql"
  res <- Wreq.asJSON @_ @Aeson.Value =<< Wreq.post (Text.unpack graphqlUrl) (encodeQuery q)
  pure (res ^.. Wreq.responseBody . key "data" . key (table q) . _Array . traverse . _Object)
  
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