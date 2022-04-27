module Main where

import Imports
import Predicate

main =
  { albums: \{ id, artist_id, title } ->
    { predicate: 
        artist_id > 5 && artist_id < 10
    }
  }