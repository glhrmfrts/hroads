module Level.IO ( loadLevels
                ) where

import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.IO

data LevelBounds
  = LevelBounds Word16 Word16
  deriving (Show)

getLevelBounds :: Get LevelBounds
getLevelBounds = LevelBounds <$> getWord16be <*> getWord16be

getAllLevelsBounds :: Word16 -> Get [LevelBounds]
getAllLevelsBounds end = do
  pos <- bytesRead
  if pos >= (fromIntegral end)
    then return []
    else do b <- getLevelBounds
            bs <- getAllLevelsBounds end
            return (b : bs)

deserializeLevels :: Get Word16
deserializeLevels = do
  firstLevelStart <- getWord16be
  firstLevelSize <- getWord16be
  --bounds <- getAllLevelsBounds firstLevelStart
  return firstLevelStart

loadLevels :: FilePath -> IO ()
loadLevels filePath = do
  contents <- BL.readFile filePath
  print $ runGet deserializeLevels contents
  return ()
