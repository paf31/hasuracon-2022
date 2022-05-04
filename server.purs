module Main where

import Imports
import Supercharger

import Partial.Unsafe (unsafeCrashWith)

config :: Config -> Config
config =
  _ { albums 
      { predicate = \{ artist_id } ->
          artist_id > 5.0 && artist_id < 100.0
      } 
    }
    
-- test = unsafeCrashWith (show tracks_) where
--   tracks_ = tracks \{ album_id, name } ->
--     { limit: Just 2
--     , offset: Nothing
--     , where_: Nothing
--     , orderBy: [ asc name ]
--     }