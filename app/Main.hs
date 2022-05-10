{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Data.Text (Text)
import Init qualified
import GHC.Generics (Generic)
import Options.Generic
import Server qualified

data Command
  = Serve { config :: FilePath }
  | Init { engineUrl :: Text }
  deriving stock (Generic, Show)

instance ParseRecord Command

main :: IO ()
main = do
  command <- getRecord "graphql-supercharger"
  case command of
    Serve config ->
      Server.server config
    Init engineUrl ->
      Init.main engineUrl