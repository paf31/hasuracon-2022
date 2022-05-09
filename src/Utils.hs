module Utils where
  
import Config qualified
import Control.Monad.Error.Class (throwError)
import Data.Aeson qualified as Aeson
import Data.These (These(..))
import Dovetail (Eval)
import Dovetail.Evaluate qualified as Evaluate
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import Language.PureScript.Names qualified as Names

fromPSValue :: Evaluate.Value () -> Aeson.Value
fromPSValue (Evaluate.Constructor (Names.ProperName "Nothing") []) = Aeson.Null
fromPSValue (Evaluate.Constructor (Names.ProperName "Just") [val]) = fromPSValue val
fromPSValue (Evaluate.String s) = Aeson.String s
fromPSValue (Evaluate.Number d) = Aeson.Number (realToFrac d)
fromPSValue (Evaluate.Bool b) = Aeson.Bool b
fromPSValue _other = error "unexpected value in fromPSValue"

toPSValue :: These Config.ColumnImport Aeson.Value -> Eval () (Evaluate.Value ())
toPSValue (These (Config.ColumnImport _ _ True) Aeson.Null) = 
  pure (Evaluate.Constructor (Names.ProperName "Nothing") [])
toPSValue (These (Config.ColumnImport _ ty True) val) = 
  (Evaluate.Constructor (Names.ProperName "Just") . (:[])) <$> toNonNullPSValue ty val
toPSValue (These (Config.ColumnImport _ ty False) val) =
  toNonNullPSValue ty val
toPSValue (This (Config.ColumnImport columnName _ _)) =
  Evaluate.throwErrorWithContext (Evaluate.OtherError ("error in gql response: missing field " <> columnName))
toPSValue That{} =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: unexpected fields")

toNonNullPSValue :: ScalarType.Type -> Aeson.Value -> Eval () (Evaluate.Value ())
toNonNullPSValue ScalarType.StringTy (Aeson.String s) = 
  pure $ Evaluate.String s
toNonNullPSValue ScalarType.StringTy _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: expected string")
toNonNullPSValue ScalarType.NumberTy (Aeson.Number d) = 
  pure $ Evaluate.Number (realToFrac d)
toNonNullPSValue ScalarType.NumberTy _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: expected number")
toNonNullPSValue ScalarType.BoolTy (Aeson.Bool b) = 
  pure $ Evaluate.Bool b
toNonNullPSValue ScalarType.BoolTy _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "error in gql response: expected boolean")
