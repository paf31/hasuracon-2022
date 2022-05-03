module Translate where
  
import Autodocodec.Extended qualified as Codec
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.API.V0.Column qualified as Column
import Hasura.Backends.DataWrapper.API.V0.Expression (Expression, Operator)
import Hasura.Backends.DataWrapper.API.V0.Expression qualified as Expression
import Hasura.Backends.DataWrapper.API.V0.OrderBy qualified as OrderBy
import Hasura.Backends.DataWrapper.API.V0.Query qualified as Query
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
import Hasura.Backends.DataWrapper.API.V0.Table qualified as Table
import HasuraClient qualified

pattern And_, Or_ :: [Expression] -> Expression
pattern And_ xs = (Expression.And (Codec.ValueWrapper xs))
pattern Or_ xs = (Expression.Or (Codec.ValueWrapper xs))

pattern Not_ :: Expression -> Expression
pattern Not_ x = (Expression.Not (Codec.ValueWrapper x))

pattern Literal_ :: Scalar.Value -> Expression
pattern Literal_ x = (Expression.Literal (Codec.ValueWrapper x))

pattern Equal_, NotEqual_ :: Expression -> Expression -> Expression
pattern Equal_ x y = (Expression.Equal (Codec.ValueWrapper2 x y))
pattern NotEqual_ x y = (Expression.NotEqual (Codec.ValueWrapper2 x y))

pattern In_ :: Expression -> [Scalar.Value] -> Expression
pattern In_ x y = (Expression.In (Codec.ValueWrapper2 x y))

pattern IsNull_, IsNotNull_ :: Expression -> Expression
pattern IsNull_ x = (Expression.IsNull (Codec.ValueWrapper x))
pattern IsNotNull_ x = (Expression.IsNotNull (Codec.ValueWrapper x))

pattern Column_ :: Column.ColumnName -> Expression
pattern Column_ x = (Expression.Column (Codec.ValueWrapper x))

pattern ApplyOperator_ :: Operator -> Expression -> Expression -> Expression
pattern ApplyOperator_ op x y = (Expression.ApplyOperator (Codec.ValueWrapper3 op x y))

toHasuraQuery :: API.Query -> HasuraClient.Query
toHasuraQuery q = 
    HasuraClient.Query
      { table   = table
      , fields  = fmap HasuraClient.Column (HashMap.keys (Query.fields q))
      , where_  = fmap translateExpression (Query.where_ q)
      , orderBy = (fmap . fmap) translateOrderBy (Query.orderBy q)
      , limit   = Query.limit q
      , offset  = Query.offset q
      }
  where
    Table.TableName table = Query.from q
    
    translateOperator :: Expression.Operator -> HasuraClient.Column -> Scalar.Value -> HasuraClient.SimplePredicate
    translateOperator Expression.LessThan = HasuraClient.Lt
    translateOperator Expression.LessThanOrEqual = HasuraClient.Lte
    translateOperator Expression.GreaterThan = HasuraClient.Gt
    translateOperator Expression.GreaterThanOrEqual = HasuraClient.Gte

    translateExpression :: Expression -> HasuraClient.Predicate
    translateExpression (And_ xs)
      = HasuraClient.And (map translateExpression xs)
    translateExpression (Or_ xs)
      = HasuraClient.Or (map translateExpression xs)
    translateExpression (Not_ x)
      = HasuraClient.Not (translateExpression x)
    translateExpression other
      = HasuraClient.Atom (translateSimpleExpression other)

    translateSimpleExpression :: Expression -> HasuraClient.SimplePredicate
    translateSimpleExpression (Equal_ (Column_ c) (Literal_ v))
      = HasuraClient.Eq (translateColumn c) v
    translateSimpleExpression (NotEqual_ (Column_ c) (Literal_ v))
      = HasuraClient.Neq (translateColumn c) v
    translateSimpleExpression (In_ (Column_ c) vs)
      = HasuraClient.In (translateColumn c) vs
    translateSimpleExpression (IsNull_ (Column_ c))
      = HasuraClient.IsNull (translateColumn c)
    translateSimpleExpression (IsNotNull_ (Column_ c))
      = HasuraClient.IsNotNull (translateColumn c)
    translateSimpleExpression (ApplyOperator_ op (Column_ c) (Literal_ v))
      = translateOperator op (translateColumn c) v
    translateSimpleExpression other
      = error $ "Unsupported expression: " <> show other

    translateColumn :: Column.ColumnName -> HasuraClient.Column
    translateColumn = HasuraClient.Column . Column.unColumnName

    translateOrderBy :: OrderBy.OrderBy -> (HasuraClient.Column, HasuraClient.OrderDirection)
    translateOrderBy (OrderBy.OrderBy (Column.ColumnName col) dir) = 
      ( HasuraClient.Column col
      , case dir of
          OrderBy.Ascending -> HasuraClient.Asc
          OrderBy.Descending -> HasuraClient.Desc
      )