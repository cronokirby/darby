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

import System.Directory (listDirectory)


-- | Represents a song which can be played
newtype Song = Song { songPath :: FilePath }

-- | Represents a playlist of songs
newtype PlayList = PlayList [Song]


isMP3File :: FilePath -> Bool
isMP3File str = isPrefixOf "3pm." (reverse str)

readPlaylist :: MonadIO m => FilePath -> m PlayList
readPlaylist dir =
    PlayList . map Song . filter isMP3File
    <$> liftIO (listDirectory dir)
