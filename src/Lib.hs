{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Lib
  ( main
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Proxy
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Aeson qualified as JSON 
import Dovetail.Evaluate qualified as Evaluate 
import Dovetail.Prelude (stdlib)
import GHC.Generics (Generic)
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Label qualified as Label
import Language.PureScript.PSString qualified as PSString
import Language.PureScript.Names qualified as Names
import Hasura.Backends.DataWrapper.API hiding (limit, offset)
import Hasura.Backends.DataWrapper.API.V0.Query qualified as Query
import Network.Wai
import Network.Wai.Handler.Warp
import Servant (Server)
import Servant.API
import Servant.Server (Handler, err500, errBody, serve)

api :: Proxy Api
api = Proxy

checkTypeOfMain 
  :: P.SourceType
  -> (forall i o. JSON.Serializable ctx o => Proxy o -> [P.RowListItem P.SourceAnn] -> Eval ctx r)
  -> Eval ctx r
checkTypeOfMain ty f 
  | P.TypeApp _ (P.TypeApp _ fn _) ty1 <- ty
  , fn == P.tyFunction
  , P.TypeApp _ arr ty2 <- ty1
  , arr == P.tyArray
  , P.TypeApp _ rec_ ty3 <- ty2
  , rec_ == P.tyRecord
  = JSON.reify ty2 \proxy ->
      let (knownFields, _) = P.rowToSortedList ty3
       in f proxy knownFields
checkTypeOfMain (P.ForAll _ _ _ ty _) f =
  checkTypeOfMain ty f
checkTypeOfMain _ _ =
  throwErrorWithContext (Evaluate.OtherError "main must have type {} -> Array (Record r)")

newtype WrappedMaybe a = WrappedMaybe (Maybe a)
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

data QueryRequest = QueryRequest
  { limit :: WrappedMaybe Integer
  , offset :: WrappedMaybe Integer
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToValue ctx)

main :: IO ()
main = do
  moduleText <- Text.readFile "server.purs"

  runInterpretWithDebugger () do
    traverse_ ffi stdlib
    
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    (query, ty) <- eval (Just moduleName) "main"
    liftEval $ checkTypeOfMain ty \(_ :: Proxy ty) fields -> do
      let server :: Server Api
          server = getSchema :<|> runQuery
          
          getSchema :: Handler SchemaResponse
          getSchema = pure (SchemaResponse caps [tableInfo])
          
          fromPSType :: P.SourceType -> Type
          fromPSType ty 
            | ty == P.tyInt = NumberTy
            | ty == P.tyNumber = NumberTy
            | ty == P.tyString = StringTy
            | ty == P.tyBoolean = BoolTy
            | otherwise = error $ "bad field type: " <> show ty
          
          tableInfo :: TableInfo
          tableInfo = TableInfo
            { dtiName        = TableName "supercharger"
            , dtiColumns     = [ ColumnInfo 
                                 { dciName = ColumnName (maybe (error "bad field name") id (PSString.decodeString fieldName))
                                 , dciType = fromPSType fieldType
                                 , dciNullable = False
                                 , dciDescription = Nothing
                                 }
                               | P.RowListItem _ (Label.Label fieldName) fieldType <- fields 
                               ]
            , dtiPrimaryKey  = Nothing
            , dtiDescription = Nothing
            }
          
          caps :: Capabilities
          caps = Capabilities 
            { dcRelationships = False 
            }
          
          runQuery :: Query -> Handler QueryResponse
          runQuery q = do
            let req = QueryRequest 
                  { limit = WrappedMaybe (fmap fromIntegral (Query.limit q))
                  , offset = WrappedMaybe (fmap fromIntegral (Query.offset q))
                  }
            response <- liftIO . runEval () $ fromValueRHS @() @(QueryRequest -> Eval () (Vector ty)) query req
            case response of
              Left err -> do
                -- todo : print error
                throwError $ err500 { errBody = "Unexpected error during evaluation, see logs." }
              Right rows -> do
                let getObject :: Aeson.Value -> Handler Aeson.Object
                    getObject (Aeson.Object o) = pure o
                    getObject _ = throwError $ err500 { errBody = "Unexpected error during evaluation, see logs." }
                QueryResponse <$> traverse (getObject . Aeson.toJSON) (Vector.toList rows)
          
          app :: Application
          app = serve api server
      
      liftIO $ run 8081 app