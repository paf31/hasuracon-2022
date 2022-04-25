module Program where
  
import Data.Text (Text)
import Data.Text qualified as Text
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy
import Dovetail
import Dovetail.Aeson qualified as JSON 
import Dovetail.Evaluate qualified as Evaluate
import GHC.TypeLits
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn

data TableCustomization = TableCustomization
  { 
  }

inferTables
  :: P.SourceType
  -> HashMap Text (Value ctx)
  -> HashMap Text TableCustomization
inferTables ty val = error "TODO"