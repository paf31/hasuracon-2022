module Main where

import Imports
import Supercharger

config :: Config -> Config
config =
  _ { albums = \{ artist_id } ->
      { predicate: 
          artist_id > 5.0 && artist_id < 10.0
      }
    }