module Config where

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as ScalarType

data Config = Config
  { source :: FilePath
  , imports :: HashMap Text URLImport
  , engineUrl :: Text
  , tables :: [TableImport]
  } deriving (Show, Generic)
    deriving anyclass Aeson.FromJSON

data URLImport = URLImport
  { uri :: String
  , response :: Text
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data TableImport = TableImport
  { name :: Text
  , columns :: [ColumnImport]
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)
    
data ColumnImport = ColumnImport
  { name :: Text
  , dataType :: ScalarType.Type
  } deriving (Show, Generic)
    deriving anyclass (Aeson.FromJSON)