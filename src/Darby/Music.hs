{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description: Contains functionality to play music.

Since playing music requires a specific context,
this module exports an opaque monad in which to play music.
-}
module Darby.Music
    ( MusicM
    , runMusicM
    , playSong
    , playPlaylist
    )
where

import Relude

import Data.Text (justifyRight)
import qualified SDL
import qualified SDL.Mixer as Mix
import qualified System.Console.ANSI as ANSI

import Darby.Playlist (Song(..), Playlist, getPlaylist)


newtype MusicM a = MusicM (IO a)
    deriving (Functor, Applicative, Monad, MonadIO)


{- | Run a musical computation

This will initialise and tear down all the sdl components,
so this should ideally be run only once.
-}
runMusicM :: MusicM a -> IO a
runMusicM (MusicM action) = do
    SDL.initialize [SDL.InitAudio]
    a <- Mix.withAudio Mix.defaultAudio 256 action
    SDL.quit
    return a


-- | Takes a duration and outputs mm:ss format
durationText :: Integer -> Text
durationText int =
    show (div int 60) 
    <> ":" 
    <> justifyRight 2 '0' (show (mod int 60))
    

{- | Prints the completion status

Takes the total duration of the song, and the current
amount of seconds passed.
-}
printCompletion :: MonadIO m => Integer -> Integer -> m ()
printCompletion full current = do
    let fullText = durationText full
        currentText = durationText current
    putTextLn (currentText <> " / " <> fullText)

-- | Plays a song until completion, blocking the thread
playSong :: Song -> MusicM ()
playSong song = do
    music <- Mix.load (songPath song)
    putTextLn ("Playing: " <> songName song)
    Mix.playMusic Mix.Once music
    delayWhile Mix.playingMusic updateCompletion
    Mix.free music
  where
    updateCompletion i = do
        printCompletion (songDuration song) i
        liftIO $ ANSI.cursorUpLine 1

playPlaylist :: Playlist -> MusicM ()
playPlaylist = mapM_ playSong . getPlaylist


-- | Keeps spinning until the condition is false
delayWhile :: MonadIO m => m Bool -> (Integer -> m ()) -> m ()
delayWhile check action = loop 0
  where
    loop i = do
        still <- check
        when still $ do
            action i
            SDL.delay 1000
            loop (i + 1)
