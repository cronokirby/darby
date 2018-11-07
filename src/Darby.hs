module Darby
    ( runDarby
    )
where

import Relude

import Darby.Context (ContextM, Context(..), runCLIContextM)
import Darby.Playlist (displayPlaylist, readPlaylist, shuffle)



runDarby :: IO ()
runDarby = runCLIContextM darby

-- | Entry point into the main program
darby :: ContextM ()
darby = do
    dir <- asks contextDirectory
    putText "Shuffling "
    putStrLn (dir ++ "...")
    playlist <- readAndShuffle dir
    displayPlaylist playlist
  where
    readAndShuffle = readPlaylist >=> shuffle