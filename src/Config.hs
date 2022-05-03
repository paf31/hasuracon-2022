module Config where

import Data.Aeson ((.:), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType

data Config = Config
  { source :: FilePath
  , imports :: HashMap Text URLImport
  , engineUrl :: Text
  , tables :: HashMap Text TableImport
  } deriving (Show, Generic)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "config" \o -> Config
    <$> o .: "source"
    <*> o .:? "imports" .!= mempty
    <*> o .: "engineUrl"
    <*> o .: "tables"

data URLImport = URLImport
  { uri :: String
  , response :: Text
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data TableImport = TableImport
  { columns :: [ColumnImport]
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data ColumnImport = ColumnImport
  { name :: Text
  , dataType :: ScalarType.Type
  , nullable :: Bool
  } deriving (Show, Generic)

instance Aeson.FromJSON ColumnImport where
  parseJSON = Aeson.withObject "column" \o -> ColumnImport
    <$> o .: "name"
    <*> o .: "type"
    <*> o .:? "nullable" .!= False