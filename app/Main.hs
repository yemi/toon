{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text, unpack)
import qualified Network.MPD                as MPD
import           Network.Wreq
import           System.Console.Haskeline   (InputT (), defaultSettings,
                                             getInputLine, outputStrLn,
                                             runInputT)

type Repl a = InputT IO a

clientID :: Text
clientID = "f0b4138275ee2a59e4017654e9ff7c3f"

getTrack :: String -> IO ()
getTrack url = do
  let opts = defaults & param "client_id" .~ [clientID]
  res <- getWith opts url
  BS.putStrLn $ res ^. responseBody

repl :: Repl ()
repl = do
  trackURL <- getInputLine "SoundCloud track URL:"
  case trackURL of
    Nothing -> outputStrLn "Goodbye."
    Just trackURL' -> do
      liftIO $ getTrack trackURL'
      liftIO . MPD.withMPD $ do
        -- MPD.add ("https://api.soundcloud.com/tracks/13158665/stream?" ++ clientID)
        MPD.add "https://api.soundcloud.com/tracks/13158665/stream?client_id=f0b4138275ee2a59e4017654e9ff7c3f"
        MPD.play Nothing
        -- MPD.currentSong
      return ()
      -- repl

main :: IO ()
main = runInputT defaultSettings repl

