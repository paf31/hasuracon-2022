module Main where

import Imports
import Supercharger

import Data.String

config :: Config _ _ _
config = defaults
  { albums 
    { predicate = \{ artist_id } ->
        artist_id > 5.0 && artist_id < 100.0
    , extras = \{ title } ->
        { foo: (getUser { user: "paf31" }).stargazers_count
        }
    } 
  }
    
-- test = unsafeCrashWith (show tracks_) where
--   tracks_ = tracks \{ album_id, name } ->
--     { limit: Just 2
--     , offset: Nothing
--     , where_: Nothing
--     , orderBy: [ asc name ]
--     } 