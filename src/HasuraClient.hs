module HasuraClient where 

import Control.Lens ((^..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Array, _Object)
import Data.Aeson.Types qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import GHC.Generics
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import Network.Wreq qualified as Wreq
import System.Exit (die)

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
  
schemaQueryText :: Text
schemaQueryText =
  "query {\
  \  __schema {\
  \    queryType {\
  \      fields {\
  \        name\
  \        args { name }\
  \        type {\
  \          kind\
  \          ofType {\
  \            kind\
  \            ofType {\
  \              kind\
  \              ofType {\
  \                kind\
  \                fields {\
  \                  name\
  \                  type {\
  \                    kind name ofType { kind name }\
  \                  }\
  \                }\
  \              }\
  \            }\
  \          }\
  \        }\
  \      }\
  \    }\
  \  }\
  \}"
  
schemaJsonOptions :: Aeson.Options
schemaJsonOptions = Aeson.defaultOptions 
  { Aeson.fieldLabelModifier = drop 1 
  , Aeson.omitNothingFields  = True
  }
  
data SchemaResponseTableArg = SchemaResponseTableArg 
  { _name :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON SchemaResponseTableArg where
  parseJSON = Aeson.genericParseJSON schemaJsonOptions

instance Aeson.ToJSON SchemaResponseTableArg where
  toJSON = Aeson.genericToJSON schemaJsonOptions
  
data SchemaResponseTableType = SchemaResponseTableType
  { _kind :: Text
  , _name :: Maybe Text
  , _ofType :: Maybe SchemaResponseTableType
  , _fields :: Maybe [SchemaResponseTableField]
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON SchemaResponseTableType where
  parseJSON = Aeson.genericParseJSON schemaJsonOptions
  
instance Aeson.ToJSON SchemaResponseTableType where
  toJSON = Aeson.genericToJSON schemaJsonOptions
  
data SchemaResponseTableField = SchemaResponseTableField
  { _name :: Text
  , _type :: SchemaResponseTableType
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON SchemaResponseTableField where
  parseJSON = Aeson.genericParseJSON schemaJsonOptions
  
instance Aeson.ToJSON SchemaResponseTableField where
  toJSON = Aeson.genericToJSON schemaJsonOptions
  
data SchemaResponseTable = SchemaResponseTable 
  { _name :: Text
  , _args :: Maybe [SchemaResponseTableArg]
  , _type :: SchemaResponseTableType
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON SchemaResponseTable where
  parseJSON = Aeson.genericParseJSON schemaJsonOptions
  
instance Aeson.ToJSON SchemaResponseTable where
  toJSON = Aeson.genericToJSON schemaJsonOptions
  
data Schema = Schema
  { tables :: [SchemaTable] 
  }
  deriving stock (Show, Eq, Generic)

data SchemaTable = SchemaTable 
  { name :: Text
  , columns :: [SchemaColumn]
  }
  deriving stock (Show, Eq, Generic)
  
data SchemaColumn = SchemaColumn
  { name :: Text
  , dataType :: ScalarType.Type
  , nullable :: Bool
  }
  deriving stock (Show, Eq, Generic)
  
getSchema :: Text -> IO Schema
getSchema engineUrl = do
  let graphqlUrl = engineUrl <> "/v1/graphql"
  res <- Wreq.asJSON @_ @Aeson.Value =<< Wreq.post (Text.unpack graphqlUrl) (Aeson.object
    [ "query"     .= schemaQueryText
    ])
  let rawFields = 
        res ^.. Wreq.responseBody 
              . key "data" 
              . key "__schema" 
              . key "queryType" 
              . key "fields" 
              . _Array 
              . traverse 
  rawTables <- 
    case Aeson.parseEither (traverse (Aeson.parseJSON @SchemaResponseTable)) rawFields of
      Left err ->
        die err
      Right rawTables ->
        pure rawTables
  let extractColumns 
        (SchemaResponseTableType "NON_NULL" _ 
          (Just 
            (SchemaResponseTableType "LIST" _ 
              (Just 
                (SchemaResponseTableType "NON_NULL" _ 
                  (Just 
                    (SchemaResponseTableType "OBJECT" _ _ (Just fields)))
                  Nothing))
              Nothing))
          Nothing) = mapMaybe extractColumn fields
      extractColumns _ = []
      
      extractColumn 
        (SchemaResponseTableField name 
          (SchemaResponseTableType "NON_NULL" _
            (Just 
              (SchemaResponseTableType "SCALAR" (Just scalarName) _ _))
            Nothing)) 
        = (\ty -> SchemaColumn name ty False) <$> fromScalarName scalarName
      extractColumn 
        (SchemaResponseTableField name 
          (SchemaResponseTableType "SCALAR" (Just scalarName) _ _))
        = (\ty -> SchemaColumn name ty True) <$> fromScalarName scalarName
      extractColumn _ = Nothing
      
      fromScalarName "String"  = Just ScalarType.StringTy
      fromScalarName "Number"  = Just ScalarType.NumberTy
      fromScalarName "Int"     = Just ScalarType.NumberTy -- can we internally distinguish an int type?
      fromScalarName "Boolean" = Just ScalarType.BoolTy
      fromScalarName _         = Nothing

  let tables = 
        [ SchemaTable name cols
        | SchemaResponseTable name args ty <- rawTables
        , args == Just
            [ SchemaResponseTableArg "distinct_on"
            , SchemaResponseTableArg "limit"
            , SchemaResponseTableArg "offset"
            , SchemaResponseTableArg "order_by"
            , SchemaResponseTableArg "where"
            ]
        , let cols = extractColumns ty
        , not (null cols)
        ]
  pure (Schema tables)