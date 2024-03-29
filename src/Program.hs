{-# LANGUAGE TemplateHaskell #-}

module Program where

import Config qualified
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
import Language.PureScript.Label qualified as Label

superchargerStaticModule :: Text
superchargerStaticModule = $(embedStringFile "purs/Supercharger.purs")

superchargerModule :: HashMap Text Config.TableImport -> Text
superchargerModule tables = 
    Text.unlines
      [ superchargerStaticModule
      , ""
      , "type Config " <> typeVars <> " = " <> makeConfigType tables
      , ""
      , "foreign import defaults :: Config " <> Text.unwords [ "()" | _ <- HashMap.keys tables]
      ]
  where
    typeVars = Text.unwords [ "_" <> tableName <> "_extras" | tableName <- HashMap.keys tables]

makeConfigType :: HashMap Text Config.TableImport -> Text
makeConfigType tables = 
    "{ " <> Text.intercalate ", " [ name <> " :: " <> makeTableConfigType name table | (name, table) <- HashMap.toList tables ] <> " }"
  where
    makeTableConfigType :: Text -> Config.TableImport -> Text
    makeTableConfigType tableName (Config.TableImport columns) =
      Text.concat
        [ "{ predicate :: { "
        , Text.intercalate ", " 
            [ name <> " :: Column " <> makeColumnType c 
            | c@(Config.ColumnImport name _ _) <- columns 
            ]
        , " } -> Predicate, extras :: {"
        , Text.intercalate ", " 
            [ name <> " :: " <> makeColumnType c 
            | c@(Config.ColumnImport name _ _) <- columns 
            ]
        , "} -> Record _"
        , tableName
        , "_extras }"
        ]
      
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
  { predicate :: HashMap Text Text -> Eval () (Evaluate.ForeignType HasuraClient.Predicate)
  , extras :: HashMap Text (Value ()) -> Eval () (HashMap Text (Value ()))
  }
  deriving stock Generic
  deriving anyclass (ToValue ())

data EvaluatedTableConfig = EvaluatedTableConfig
  { tableType :: TableConfigType
  , predicate :: HasuraClient.Predicate
  , extras :: HashMap Text (Value ()) -> Eval () (HashMap Text (Value ()))
  }

makeDefaultConfig :: HashMap Text Config.TableImport -> HashMap Text TableConfig
makeDefaultConfig = HashMap.map makeDefaultTableConfig where
  makeDefaultTableConfig :: Config.TableImport -> TableConfig
  makeDefaultTableConfig _ =
    TableConfig 
      { predicate = \_ -> pure (Evaluate.ForeignType (HasuraClient.And []))
      , extras = \_ -> pure HashMap.empty
      }

data ExtraColumn = ExtraColumn
  { type_ :: ScalarType.Type
  , nullable :: Bool
  }

data TableConfigType = TableConfigType
  { extraColumns :: HashMap Text ExtraColumn
  }

extractTableConfigTypes :: P.SourceType -> Eval () (HashMap Text TableConfigType)
extractTableConfigTypes ty =
  case ty of
    P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Record"))) ty -> do
      let (knownFields, _) = P.rowToSortedList ty
          
          extractFromTableType :: P.RowListItem P.SourceAnn -> Eval () (Text, TableConfigType)
          extractFromTableType (P.RowListItem _ (Label.Label tableName') ty) = do
            tableName <- Evaluate.evalPSString tableName' 
            case ty of
              P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Record"))) ty' -> do
                let (tableConfigFields, _) = P.rowToSortedList ty'
                let fromTableField (P.RowListItem _ (Label.Label name) ty) = 
                      (,) <$> Evaluate.evalPSString name 
                          <*> pure ty
                tableConfigFields <- traverse fromTableField tableConfigFields
                case lookup "extras" tableConfigFields of
                  Just extrasType -> do
                    extraColumns <- extractFromExtrasType extrasType
                    pure (tableName, TableConfigType extraColumns)
                  Nothing ->
                    Evaluate.throwErrorWithContext $ Evaluate.OtherError "Table configuration is missing the 'extras' field"  
              _ ->
                Evaluate.throwErrorWithContext $ Evaluate.OtherError "Table configuration must be a record"
          
          extractFromExtrasType :: P.SourceType -> Eval () (HashMap Text ExtraColumn)
          extractFromExtrasType (P.TypeApp _ (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Function"))) _) cod) =
            case cod of
              P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Record"))) ty'' -> do
                let (extrasFields, _) = P.rowToSortedList ty''
                HashMap.fromList <$> traverse fromExtrasField extrasFields
              _ ->
                Evaluate.throwErrorWithContext $ Evaluate.OtherError "Table configuration 'extras' function must return a record"
          extractFromExtrasType _ =
            Evaluate.throwErrorWithContext $ Evaluate.OtherError "Table configuration 'extras' field must be a function"
          
          fromExtrasField :: P.RowListItem P.SourceAnn -> Eval () (Text, ExtraColumn)
          fromExtrasField (P.RowListItem _ (Label.Label name) ty) = 
            (,) <$> Evaluate.evalPSString name 
                <*> fromSourceType ty
                
          fromSourceType :: P.SourceType -> Eval () ExtraColumn
          fromSourceType (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Data.Maybe")) (P.ProperName "Maybe"))) ty) =
            ExtraColumn <$> fromPrimSourceType ty <*> pure True
          fromSourceType ty =
            ExtraColumn <$> fromPrimSourceType ty <*> pure False  
            
          fromPrimSourceType :: P.SourceType -> Eval () ScalarType.Type
          fromPrimSourceType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Number"))) =
            pure ScalarType.NumberTy
          fromPrimSourceType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Int"))) =
            pure ScalarType.NumberTy
          fromPrimSourceType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "String"))) =
            pure ScalarType.StringTy
          fromPrimSourceType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Boolean"))) =
            pure ScalarType.BoolTy
          fromPrimSourceType _ =
            Evaluate.throwErrorWithContext $ Evaluate.OtherError "field in extras config is not serializable"
      HashMap.fromList <$> traverse extractFromTableType knownFields
    _ -> 
      Evaluate.throwErrorWithContext $ Evaluate.OtherError "Config must be a record of tables"

evalConfig 
  :: Config.Config
  -> ModuleName
  -> Interpret () (HashMap Text EvaluatedTableConfig)
evalConfig config moduleName = do
  let configExpr = "config :: Supercharger.Config " <> Text.unwords [ "_" | _ <- HashMap.keys (Config.tables config)]
  (untypedConfig, ty) <- eval @_ @(Eval () (Value ())) (Just moduleName) configExpr
  tableTypes <- liftEval $ extractTableConfigTypes ty
  liftEval do
    evaluatedConfig <- untypedConfig
    case evaluatedConfig of
      Evaluate.Object tables -> do
        let evalTableConfig tbl val = do 
              cols <- case HashMap.lookup tbl (Config.tables config) of
                        Just Config.TableImport { columns } -> do
                          pure (HashMap.fromList [ (columnName, columnName) | Config.ColumnImport columnName _ _ <- columns ])
                        Nothing -> 
                          Evaluate.throwErrorWithContext $ Evaluate.OtherError "Table must be configured in config.yaml file"
              tableType <- case HashMap.lookup tbl tableTypes of
                             Just tableType -> do
                               pure tableType
                             Nothing -> 
                               Evaluate.throwErrorWithContext $ Evaluate.OtherError "Unable to find table in runtime configuration"
              TableConfig mkPredicate mkExtras <- fromValue @_ @TableConfig val
              Evaluate.ForeignType predicate <- mkPredicate cols
              pure (EvaluatedTableConfig tableType predicate mkExtras)
        HashMap.traverseWithKey evalTableConfig tables
      _ -> 
        Evaluate.throwErrorWithContext $ Evaluate.OtherError "Config must be a record of tables"

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
          
    , Evaluate.builtIn @() @(HashMap Text TableConfig)
        (P.ModuleName "Supercharger") "defaults" 
        (makeDefaultConfig tables)
    ]
    
  build $ superchargerModule tables