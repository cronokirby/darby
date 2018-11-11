module Darby
    ( runDarby
    )
where

import Relude

import Darby.Context (ContextM, Context(..), runCLIContextM)
import Darby.Music (runMusicM, playPlaylist)
import Darby.Playlist (Playlist, displayPlaylist, readPlaylist, 
                       shufflePlaylist)


runDarby :: IO ()
runDarby = runCLIContextM darby

-- | Entry point into the main program
darby :: ContextM ()
darby = do
    dir <- asks contextDirectory
    shuffle <- asks contextShuffle
    playlist <- if shuffle
        then playShuffle dir
        else playNoShuffle dir
    display <- asks contextJustDisplay
    if display
        then displayPlaylist playlist
        else liftIO . runMusicM $ playPlaylist playlist
  where
    readAndShuffle = readPlaylist >=> shufflePlaylist
    playShuffle :: FilePath -> ContextM Playlist
    playShuffle dir = do
        putText "Shuffling "
        putStrLn (dir ++ "...")
        readAndShuffle dir
    playNoShuffle :: FilePath -> ContextM Playlist
    playNoShuffle dir = do
        putStrLn $ "Playing the songs from "
                ++ dir ++ " in order..."
        readPlaylist dir
    