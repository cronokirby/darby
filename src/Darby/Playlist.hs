{-|
Description: Contains functionality related to songs and playlists

Exports opaque types, with the intention of
being accessed with black box methods, like shuffle,
or play.
-}
module Darby.Playlist 
    ( readPlaylist
    )
where

import Relude

import Data.Array.IO (IOArray, newListArray, 
                      readArray, writeArray)
import System.Directory (listDirectory)
import System.Random (randomRIO)


-- | Represents a song which can be played
newtype Song = Song { songPath :: FilePath }

-- | Represents a playlist of songs
newtype PlayList = PlayList [Song]


isMP3File :: FilePath -> Bool
isMP3File str = isPrefixOf "3pm." (reverse str)

{- | Reads a playlist from a base directory

This works by listing all files in the directory,
and filtering out everything that isn't an mp3 file.
This should be extended in the future to all music
files that we support.
-}
readPlaylist :: MonadIO m => FilePath -> m PlayList
readPlaylist dir =
    PlayList . map Song . filter isMP3File
    <$> liftIO (listDirectory dir)


shuffle :: MonadIO m => PlayList -> m PlayList
shuffle (PlayList songs) = fmap PlayList . liftIO $ do
    arr <- newArray ln songs
    forM [1..ln] $ \i -> do
        j  <- randomRIO (i, ln)
        vi <- readArray arr i
        vj <- readArray arr j
        writeArray arr j vi
        return vj
  where
    ln = length songs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = liftIO $ newListArray (1, n) xs
