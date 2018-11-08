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

import qualified SDL
import qualified SDL.Mixer as Mix

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


-- | Plays a song until completion, blocking the thread
playSong :: Song -> MusicM ()
playSong song = do
    music <- Mix.load (songPath song)
    putTextLn ("Playing: " <> songName song)
    Mix.playMusic Mix.Once music
    delayWhile Mix.playingMusic
    Mix.free music

playPlaylist :: Playlist -> MusicM ()
playPlaylist = mapM_ playSong . getPlaylist


-- | Keeps spinning until the condition is false
delayWhile :: MonadIO m => m Bool -> m ()
delayWhile check = loop
  where
    loop = do
        still <- check
        when still $ do
            SDL.delay 300
            loop
