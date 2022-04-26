module Main where

import Imports
import Predicate

main =
  { albums: \{ id, artist_id, title } ->
    { predicate: 
        artist_id `in_` [1, 2, 3]
    }
  }