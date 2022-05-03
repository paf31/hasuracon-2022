{-# LANGUAGE TemplateHaskell #-}

module Program where

import Config qualified
import Control.Monad.IO.Class (liftIO)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import GHC.Generics (Generic)
import HasuraClient qualified
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as Scalar
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
import Language.PureScript qualified as P
import System.Exit (die)

superchargerStaticModule :: Text
superchargerStaticModule = $(embedStringFile "purs/Supercharger.purs")

superchargerModule :: HashMap Text Config.TableImport -> Text
superchargerModule tables = Text.unlines
  [ superchargerStaticModule
  , ""
  , "type Config = " <> makeConfigType tables
  ]

makeConfigType :: HashMap Text Config.TableImport -> Text
makeConfigType tables = 
    "{ " <> Text.intercalate ", " [ name <> " :: " <> makeTableConfigType table | (name, table) <- HashMap.toList tables ] <> " }"
  where
    makeTableConfigType :: Config.TableImport -> Text
    makeTableConfigType (Config.TableImport columns) =
      "{ " <> Text.intercalate ", " [ name <> " :: Column " <> makeColumnType c | c@(Config.ColumnImport name _ _) <- columns ] <> " } -> { predicate :: Predicate }"
      
    makeColumnType :: Config.ColumnImport -> Text
    makeColumnType (Config.ColumnImport _ ty nullable) = 
      let baseType = 
            case ty of
              ScalarType.StringTy -> "String"
              ScalarType.NumberTy -> "Number"
              ScalarType.BoolTy   -> "Boolean"
       in if nullable
            then "(Maybe " <> baseType <> ")"
            else baseType

data TableConfig = TableConfig
  { predicate :: Evaluate.ForeignType HasuraClient.Predicate
  }
  deriving stock Generic
  deriving anyclass (ToValue ctx)

makeDefaultConfig :: HashMap Text Config.TableImport -> Evaluate.Value ()
makeDefaultConfig = Evaluate.Object . HashMap.map (Evaluate.toValue . makeDefaultTableConfig) where
  makeDefaultTableConfig :: Config.TableImport -> Evaluate.Value () -> Evaluate.Eval () TableConfig
  makeDefaultTableConfig _ _ =
    pure TableConfig 
      { predicate = Evaluate.ForeignType (HasuraClient.And [])
      }

evalConfig 
  :: Config.Config
  -> ModuleName
  -> Interpret () (HashMap Text TableConfig)
evalConfig config moduleName = do
  let defaultConfig = makeDefaultConfig (Config.tables config)
  (untypedConfig, _ty) <- eval @_ @(Value () -> Eval () (Value ())) (Just moduleName) "config :: Supercharger.Config -> Supercharger.Config"
  liftEval do
    evaluatedConfig <- liftIO $ runEval () (untypedConfig defaultConfig)
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

install :: HashMap Text Config.TableImport -> Interpret () (Module Ann)
install tables = do
  let toScalar (Evaluate.String s) = Scalar.String s
      toScalar (Evaluate.Int i) = Scalar.Number (fromIntegral i)
      toScalar (Evaluate.Number d) = Scalar.Number (realToFrac d)
      toScalar (Evaluate.Bool b) = Scalar.Boolean b
      toScalar _ = error "cannot convert value"
  loadEnv $ fold
    [ Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_eq" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Eq (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_neq" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Neq (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Vector (Value ()) -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_in" 
        \col vals -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.In (HasuraClient.Column col) (map toScalar (Vector.toList vals)))))
      
    , Evaluate.builtIn @() @(Text -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "isNull" 
        \col -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.IsNull (HasuraClient.Column col))))
      
    , Evaluate.builtIn @() @(Text -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "isNotNull" 
        \col -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.IsNotNull (HasuraClient.Column col))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_lt" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Lt (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_lte" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Lte (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_gt" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Gt (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Text -> Value () -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_gte" 
        \col val -> do
          pure (Evaluate.ForeignType (HasuraClient.Atom (HasuraClient.Gte (HasuraClient.Column col) (toScalar val))))
      
    , Evaluate.builtIn @() @(Vector (Evaluate.ForeignType HasuraClient.Predicate) -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_and" 
        \xs -> do
          pure (Evaluate.ForeignType (HasuraClient.And (map Evaluate.getForeignType (Vector.toList xs))))
      
    , Evaluate.builtIn @() @(Vector (Evaluate.ForeignType HasuraClient.Predicate) -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "_or" 
        \xs -> do
          pure (Evaluate.ForeignType (HasuraClient.Or (map Evaluate.getForeignType (Vector.toList xs))))
      
    , Evaluate.builtIn @() @(Evaluate.ForeignType HasuraClient.Predicate -> Eval () (Evaluate.ForeignType HasuraClient.Predicate))
        (P.ModuleName "Supercharger") "not" 
        \p -> do
          pure (Evaluate.ForeignType (HasuraClient.Not (Evaluate.getForeignType p)))
          
    , Evaluate.builtIn @() @(Text -> Eval () (Evaluate.ForeignType (HasuraClient.Column, HasuraClient.OrderDirection)))
        (P.ModuleName "Supercharger") "asc" 
        \c -> do
          pure (Evaluate.ForeignType (HasuraClient.Column c, HasuraClient.Asc))
          
    , Evaluate.builtIn @() @(Text -> Eval () (Evaluate.ForeignType (HasuraClient.Column, HasuraClient.OrderDirection)))
        (P.ModuleName "Supercharger") "desc" 
        \c -> do
          pure (Evaluate.ForeignType (HasuraClient.Column c, HasuraClient.Desc))
    ]
    
  build $ superchargerModule tables