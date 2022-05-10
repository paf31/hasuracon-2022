module Main where

import Imports
import Supercharger

import Prelude hiding ((<), (>), (&&), (==))
import Prelude as P
import Data.Array
import Data.String
import Data.Maybe (Maybe(..), maybe)
import JSON

config :: Config _ _ _
config = defaults
  { albums 
    { predicate = \{ artist_id } ->
        artist_id > 5.0 && artist_id < 100.0
    , extras = \{ title } ->
        let query = "name:" <> title
            { releases } = musicbrainz { query }
         in { mbid: _.id <$> head releases }
    } 
  }