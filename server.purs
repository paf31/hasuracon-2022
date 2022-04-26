module Main where

import Filter
import Imports

main =
  { albums: \{ id, artist_id, title } ->
    { predicate: 
        artist_id `eq` 1
    }
  }