module Main where

import Client
import Imports
import Supercharger

import Prelude.Debug

config :: Config -> Config
config =
  _ { albums = \{ artist_id } ->
      { predicate: 
          artist_id > 5.0 && artist_id < 10.0
      }
    }
    
test = crash (show (albums \{ artist_id } ->
  { limit: 10
  , offset: 0
  , where_: artist_id == 5.0
  }))