module Init where
  
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import HasuraClient qualified
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType
  
fromType :: ScalarType.Type -> Text
fromType ScalarType.StringTy = "string"
fromType ScalarType.NumberTy = "number"
fromType ScalarType.BoolTy = "boolean"
  
instructions :: Text -> HasuraClient.Schema -> Text
instructions engineUrl (HasuraClient.Schema tables) = Text.unlines $
    [ "cat <<EOF > config.yaml"
    , "source: server.purs"
    , "engineUrl: " <> engineUrl
    , "tables:"
    ] <> concat
    [ [ "  " <> tableName <> ":"
      , "    columns:"
      ] <> concat
      [ [ "      - name: " <> columnName
        , "        type: " <> fromType type_
        ] <>
        if nullable
          then [ "        nullable: true" ] 
          else []
      | HasuraClient.SchemaColumn columnName type_ nullable <- columns
      ]
    | HasuraClient.SchemaTable tableName columns <- tables
    ] <>
    [ "# # HTTP GET imports:"
    , "# #"
    , "# imports:"
    , "#   getGitHubRepos:"
    , "#     uri: https://api.github.com/users{/user}/repos"
    , "#     response: |"
    , "#       Prim.Array"
    , "#         { id :: Prim.Int"
    , "#         , name :: Prim.String"
    , "#         , stargazers_count :: Prim.Int"
    , "#         }"
    , "EOF"
    , ""
    ] <>
    
    [ "cat <<EOF > server.purs"
    , "module Main where"
    , ""
    , "-- import Imports (getGitHubRepos)"
    , "import Supercharger"
    , ""
    , "config :: Config " <> Text.unwords [ "_" | _ <- tables ]
    , "config = defaults"
    , "--  { " <> tableName <> " "
    , "--    { predicate = \\{ " <> columnNames <> " } ->"
    , "--        ?predicate"
    , "--    , extras = \\{ " <> columnNames <> " } ->"
    , "--        { extraField: ?resolver"
    , "--        }"
    , "--    } "
    , "--  }"
    , "EOF"
    , ""
    ] <>
    
    [ "graphql-supercharger serve --config config.yaml" ]
  where
    HasuraClient.SchemaTable tableName columns = head tables
    columnNames = Text.intercalate ", " [ columnName | HasuraClient.SchemaColumn columnName _ _ <- columns ]
  
main :: Text -> IO ()
main engineUrl = do
  schema <- HasuraClient.getSchema engineUrl
  Text.putStrLn (instructions engineUrl schema)