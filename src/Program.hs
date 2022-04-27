module Program where

import Config qualified
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import GHC.Generics (Generic)
import HasuraClient qualified
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
import Language.PureScript qualified as P
import System.Exit (die)

predicateModule :: Text
predicateModule =
  "module Predicate where\n\
  \\n\
  \foreign import data Column :: Type -> Type\n\
  \\n\
  \foreign import data Predicate :: Type\n\
  \\n\
  \foreign import eq :: forall a. Column a -> a -> Predicate\n\
  \foreign import neq :: forall a. Column a -> a -> Predicate\n\
  \foreign import in_ :: forall a. Column a -> Array a -> Predicate\n\
  \foreign import isNull :: forall a. Column a -> Predicate\n\
  \foreign import isNotNull :: forall a. Column a -> Predicate\n\
  \foreign import lt :: forall a. Column a -> a -> Predicate\n\
  \foreign import lte :: forall a. Column a -> a -> Predicate\n\
  \foreign import gt :: forall a. Column a -> a -> Predicate\n\
  \foreign import gte :: forall a. Column a -> a -> Predicate\n\
  \foreign import and :: Array Predicate -> Predicate\n\
  \foreign import or :: Array Predicate -> Predicate\n\
  \foreign import not :: Predicate -> Predicate\n\
  \\n\
  \binaryAnd :: Predicate -> Predicate -> Predicate\n\
  \binaryAnd x y = and [x, y]\n\
  \\n\
  \binaryOr :: Predicate -> Predicate -> Predicate\n\
  \binaryOr x y = or [x, y]\n\
  \\n\
  \infix 4 eq as ==\n\
  \infix 4 neq as !=\n\
  \infixl 4 lt as <\n\
  \infixl 4 lte as <=\n\
  \infixl 4 gt as >\n\
  \infixl 4 gte as >=\n\
  \infixr 3 binaryAnd as &&\n\
  \infixr 2 binaryOr as ||"

data TableConfig = TableConfig
  { predicate :: Evaluate.ForeignType HasuraClient.Predicate
  }
  deriving stock Generic
  deriving anyclass (ToValue ctx)

evalConfig 
  :: Config.Config
  -> ModuleName
  -> Text
  -> Interpret () (HashMap Text TableConfig)
evalConfig config moduleName expr = do
  -- TODO: check the type matches the config
  (untypedConfig, ty) <- eval @_ @(Eval () (Value ())) (Just moduleName) expr
  liftEval do
    evaluatedConfig <- liftIO $ runEval () untypedConfig
    case evaluatedConfig of
      Right (Evaluate.Object tables) -> do
        let evalTableConfig :: Text -> Value () -> Eval () Program.TableConfig
            evalTableConfig tbl fn =
              case HashMap.lookup tbl (Config.tables config) of
                Just Config.TableImport { columns } -> do
                  let cols = Evaluate.Object (HashMap.fromList [ (Config.name c, Evaluate.String (Config.name c)) | c <- columns ])
                  fromValue @_ @Program.TableConfig =<< Evaluate.apply fn cols
                Nothing -> liftIO $ die "table not configured"
        HashMap.traverseWithKey evalTableConfig tables
      Right{} -> 
        liftIO $ die "Config must be a record of tables"
      Left err ->
        liftIO $ die (renderEvaluationError defaultTerminalRenderValueOptions err)

install :: Interpret () ()
install = do
  _ <- build predicateModule

  let toScalar (Evaluate.String s) = Scalar.String s
      toScalar (Evaluate.Int i) = Scalar.Number (fromIntegral i)
      toScalar (Evaluate.Number d) = Scalar.Number (realToFrac d)
      toScalar (Evaluate.Bool b) = Scalar.Boolean b
      toScalar _ = error "cannot convert value"
  loadEnv $ fold
    [ Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "eq" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Eq (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "neq" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Neq (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Vector (Value ()) -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "in_" 
        \col vals -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.In (HasuraClient.Column col) (map toScalar (Vector.toList vals)))))
      
    , Evaluate.builtIn @() @(Text -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "isNull" 
        \col -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.IsNull (HasuraClient.Column col))))
      
    , Evaluate.builtIn @() @(Text -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "isNotNull" 
        \col -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.IsNotNull (HasuraClient.Column col))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "lt" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Lt (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "lte" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Lte (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "gt" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Gt (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "gte" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Gte (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Vector (Evaluate.ForeignType HasuraClient.Predicate) -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "and" 
        \xs -> do
          pure (Evaluate.ForeignType (HasuraClient.And (map Evaluate.getForeignType (Vector.toList xs))))
      
    , Evaluate.builtIn @() @(Vector (Evaluate.ForeignType HasuraClient.Predicate) -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "or" 
        \xs -> do
          pure (Evaluate.ForeignType (HasuraClient.Or (map Evaluate.getForeignType (Vector.toList xs))))
      
    , Evaluate.builtIn @() @(Evaluate.ForeignType HasuraClient.Predicate -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Predicate") "not" 
        \p -> do
          pure (Evaluate.ForeignType (HasuraClient.Not (Evaluate.getForeignType p)))
    ]