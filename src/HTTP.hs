module HTTP where

import Burrito qualified
import Burrito.Internal.Render qualified 
import Burrito.Internal.Type.Expression qualified as Burrito.Expression
import Burrito.Internal.Type.Template qualified as Burrito.Template
import Burrito.Internal.Type.Token qualified as Burrito.Token
import Burrito.Internal.Type.Variable qualified as Burrito.Variable
import Config (URLImport(..))
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text qualified as Text
import Dovetail
import Dovetail.Aeson qualified as JSON
import Dovetail.FFI.Internal qualified as FFI
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Label qualified as Label
import Language.PureScript.PSString qualified as PSString
import Network.Wreq qualified as Wreq
import System.Exit (die)
  
data ParsedURLImport = ParsedURLImport Burrito.Template P.SourceType

parseType :: Text -> Either String P.SourceType
parseType input = do
  let tokens = CST.lex input
      (_, parseResult) = CST.runParser (CST.ParserState tokens [] []) CST.parseType
  either (Left . foldMap CST.prettyPrintError) (Right . CST.convertType "<input>") parseResult

parseURLImport :: URLImport -> Either String ParsedURLImport
parseURLImport URLImport{..} = 
  ParsedURLImport
    <$> maybe (Left "unable to parse URI template") Right (Burrito.parse uri)
    <*> parseType response

importAll :: HashMap Text URLImport -> IO (Eval () [ForeignImport ()])
importAll imports = do
  parsedImports <-
    case traverse parseURLImport imports of
      Left err -> die err
      Right x -> pure x

  pure $ traverse importToFFI (HashMap.toList parsedImports)
    
importToFFI :: (Text, ParsedURLImport) -> Eval () (ForeignImport ())
importToFFI (name, ParsedURLImport url ty) = 
    JSON.reify ty \(_ :: Proxy ty) ->
      pure ForeignImport
        { fv_name = P.Ident name
        , fv_type = args `FFI.function` ty
        , fv_value = toValue @() @(HashMap Text Text -> Eval () ty) \vals -> do
            let uri = Burrito.expand [(Text.unpack k, Burrito.stringValue (Text.unpack v)) | (k, v) <- HashMap.toList vals] url
            res <- liftIO $ Wreq.asJSON @_ @ty =<< Wreq.get uri
            pure (res ^. Wreq.responseBody)
        }
  where
    args = P.TypeApp P.nullSourceAnn P.tyRecord argsRow
    argsRow = P.rowFromList ([P.srcRowListItem var P.tyString | var <- uriVars url], P.srcREmpty)
    
uriVars :: Burrito.Template -> [Label.Label]
uriVars = foldMap go . Burrito.Template.tokens where
  go :: Burrito.Token.Token -> [Label.Label]
  go (Burrito.Token.Expression o) = foldMap ((:[]) . toLabel . Burrito.Variable.name) (Burrito.Expression.variables o)
  go Burrito.Token.Literal{} = mempty 
  
  toLabel name = Label.Label (PSString.mkString (Text.pack (Burrito.Internal.Render.builderToString (Burrito.Internal.Render.name name))))
