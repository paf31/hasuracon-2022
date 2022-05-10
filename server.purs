module Main where

-- import Imports (getGitHubRepos)
import Supercharger

config :: Config _ _ _
config = defaults
 { albums 
   { predicate = \{ artist_id, id, title } ->
       artist_id == 2.0
   , extras = \{ artist_id, id, title } ->
       { extraField: "Hello, World!"
       }
   } 
 }
