{-|
Description: Contains functionality related to songs and playlists

Exports opaque types, with the intention of
being accessed with black box methods, like shuffle,
or play.
-}
module Darby.Playlist 
    ( Song(..)
    , Playlist
    , getPlaylist
    , readPlaylist
    , shufflePlaylist
    , displayPlaylist
    )
where

import Relude

import Data.Array.IO (IOArray, newListArray, 
                      readArray, writeArray)
import Data.Text (stripSuffix)
import qualified Sound.TagLib as Tag
import System.Directory (listDirectory)
import System.Random (randomRIO)


-- | Represents a song which can be played
data Song = Song 
    { songPath :: FilePath -- ^ The file for this song
    , songName :: Text -- ^ The name of this song
    , songDuration :: Integer -- ^ The seconds of the song
    }
    deriving (Eq, Show)

{- | Makes a song from a file by stripping the extension

Returns Nothing if this isn't an mp3 file
-}
makeSong :: FilePath -> FilePath -> IO (Maybe Song)
makeSong dir path = do
    let suffix = stripSuffix ".mp3" (fromString path)
    duration <- getDuration fullPath
    wrapSong suffix duration
  where
    fullPath = dir ++ "/" ++ path
    wrapSong :: Maybe Text -> Maybe Integer -> IO (Maybe Song)
    wrapSong name duration = 
        return $ Song fullPath 
        <$> name
        <*> duration
    getDuration :: FilePath -> IO (Maybe Integer)
    getDuration file = runMaybeT $ do
        tagFile <- MaybeT $ Tag.open file
        properties <- MaybeT $ Tag.audioProperties tagFile
        liftIO $ Tag.duration properties


-- | Represents a playlist of songs
newtype Playlist = Playlist 
    { getPlaylist :: [Song] 
    } 
    deriving (Eq, Show)


{- | Reads a playlist from a base directory

This works by listing all files in the directory,
and filtering out everything that isn't an mp3 file.
This should be extended in the future to all music
files that we support.
-}
readPlaylist :: MonadIO m => FilePath -> m Playlist
readPlaylist dir = do
    files <- liftIO $ listDirectory dir
    maybeSongs <- liftIO $ mapM (makeSong dir) files
    let filtered = catMaybes maybeSongs
    return (Playlist filtered)


{- | Shuffle an array using Fishery-Yates

Requires IO for random number generation, but
also uses it for array manipulation.
-}
shufflePlaylist :: MonadIO m => Playlist -> m Playlist
shufflePlaylist (Playlist songs) = fmap Playlist . liftIO $ do
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

-- | Print the names of every song in a playlist
displayPlaylist :: MonadIO m => Playlist -> m ()
displayPlaylist (Playlist songs) =
    forM_ songs $ putTextLn . songName
