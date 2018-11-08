import Relude

import System.Directory (removeFile)
import Test.Hspec

import Darby.Playlist


main :: IO ()
main = do
    makeFiles
    hspec playlistSpec
    removeFiles
  where
    files = ["a.mp3", "b.mp3", "c.notmp3"]
    makeFiles = forM_ files $ \file -> writeFile file ""
    removeFiles = forM_ files removeFile


playlistSpec :: SpecWith ()
playlistSpec =
    describe "readPlayList" $
        it "Reads the valid mp3 files in a directory" $
            readPlaylist "." `shouldReturn` 
                Playlist [Song "b.mp3" "b", Song "a.mp3" "a"]
