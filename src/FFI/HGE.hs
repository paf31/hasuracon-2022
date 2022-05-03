module FFI.HGE where

import Config qualified
import Control.Monad.IO.Class (liftIO)
import Data.Align (alignWith)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.FFI.Internal qualified as FFI
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import HasuraClient qualified
import Language.PureScript qualified as P
import Language.PureScript.Label qualified as Label
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString

data FFIQuery = FFIQuery
  { where_ :: WrappedMaybe (Evaluate.ForeignType HasuraClient.Predicate)
  , orderBy :: Vector (Evaluate.ForeignType (HasuraClient.Column, HasuraClient.OrderDirection))
  , limit  :: WrappedMaybe Integer
  , offset :: WrappedMaybe Integer
  } 
  deriving stock (Generic)
  deriving anyclass (Evaluate.ToValue ctx)

newtype WrappedMaybe a = WrappedMaybe { getWrappedMaybe :: Maybe a }
  deriving stock (Show, Eq, Generic)

instance ToValue ctx a => ToValue ctx (WrappedMaybe a) where
  toValue (WrappedMaybe Nothing) = 
    Evaluate.Constructor (Names.ProperName "Nothing") []
  toValue (WrappedMaybe (Just a)) = 
    Evaluate.Constructor (Names.ProperName "Just") [toValue a]

  fromValue (Evaluate.Constructor (Names.ProperName "Nothing") []) =
    pure (WrappedMaybe Nothing)
  fromValue (Evaluate.Constructor (Names.ProperName "Just") [val]) =
    WrappedMaybe . Just <$> fromValue val
  fromValue other =
    Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "Maybe" other)

importAll :: Config.Config -> [ForeignImport ()]
importAll config = map (tableToImport (Config.engineUrl config)) (HashMap.toList (Config.tables config))

tableToImport :: Text -> (Text, Config.TableImport) -> ForeignImport ()
tableToImport engineUrl (name, Config.TableImport columns) = 
    ForeignImport
      { fv_name = P.Ident name
      , fv_type = (cols `FFI.function` args) `FFI.function` FFI.array result
      , fv_value = toValue @() @((HashMap Text Text -> Eval () FFIQuery) -> Eval () (Vector (HashMap Text (Evaluate.Value ())))) \f -> do
          let columnsRecord = HashMap.fromList [ (col, col) | Config.ColumnImport col _ _ <- columns ]
              columnsByName = HashMap.fromList [ (col, c) | c@(Config.ColumnImport col _ _) <- columns ]
          query <- f columnsRecord
          let hasuraQuery = HasuraClient.Query
                { HasuraClient.table = name
                -- TODO: try to determine the minimum set of fields required
                , HasuraClient.fields = [ HasuraClient.Column col | Config.ColumnImport col _ _ <- columns]
                , HasuraClient.where_ = Evaluate.getForeignType <$> getWrappedMaybe (where_ query)
                , HasuraClient.orderBy = nonEmpty [ Evaluate.getForeignType o | o <- Vector.toList (orderBy query) ]
                , HasuraClient.limit = fromIntegral <$> getWrappedMaybe (limit query)
                , HasuraClient.offset = fromIntegral <$> getWrappedMaybe (offset query)
                }
          res <- liftIO $ HasuraClient.runQuery engineUrl hasuraQuery
          Vector.fromList <$> traverse (\o -> sequence (alignWith toPSValue columnsByName o)) res
      }
  where
    columnType :: Config.ColumnImport -> P.SourceType
    columnType c = 
      P.TypeApp P.nullSourceAnn 
        (P.TypeConstructor P.nullSourceAnn 
          (P.mkQualified (P.ProperName "Column") (P.ModuleName "Supercharger"))) 
        (toPSType c)
    
    toPSType :: Config.ColumnImport -> P.SourceType
    toPSType (Config.ColumnImport _ ty nullable) = 
      let baseType = 
            case ty of
              ScalarType.StringTy -> P.tyString
              ScalarType.NumberTy -> P.tyNumber
              ScalarType.BoolTy   -> P.tyBoolean
       in if nullable
            then maybeTy baseType
            else baseType
    
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
    
    cols = P.TypeApp P.nullSourceAnn P.tyRecord colsRow
    colsRow = P.rowFromList 
      ( [P.srcRowListItem (Label.Label (PSString.mkString col)) (columnType c) | c@(Config.ColumnImport col _ _) <- columns]
      , P.srcREmpty
      )
    
    args = P.TypeApp P.nullSourceAnn P.tyRecord argsRow
    argsRow = P.rowFromList 
      ( [ P.srcRowListItem (Label.Label (PSString.mkString "limit")) (maybeTy P.tyInt)
        , P.srcRowListItem (Label.Label (PSString.mkString "offset")) (maybeTy P.tyInt)
        , P.srcRowListItem (Label.Label (PSString.mkString "where_"))
            (maybeTy predicateTy)
        , P.srcRowListItem (Label.Label (PSString.mkString "orderBy"))
            (FFI.array orderTy)
        ]
      , P.srcREmpty
      )
    
    predicateTy = 
      P.TypeConstructor P.nullSourceAnn
        (P.mkQualified (P.ProperName "Predicate") (P.ModuleName "Supercharger"))
        
    orderTy = 
      P.TypeConstructor P.nullSourceAnn
        (P.mkQualified (P.ProperName "Order") (P.ModuleName "Supercharger"))
    
    maybeTy = 
      P.TypeApp P.nullSourceAnn 
        (P.TypeConstructor P.nullSourceAnn 
          (P.mkQualified (P.ProperName "Maybe") (P.ModuleName "Data.Maybe")))
    
    result = P.TypeApp P.nullSourceAnn P.tyRecord resultRow
    resultRow = P.rowFromList 
      ( [P.srcRowListItem (Label.Label (PSString.mkString col)) (toPSType c) | c@(Config.ColumnImport col _ _) <- columns]
      , P.srcREmpty
      )