{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens             ((&), (.~), (^.), (^?))
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson               (FromJSON)
import           Data.Aeson.Lens          (key, _String)
import           Data.Monoid              ((<>))
import           Data.String              (fromString)
import           Data.Text                (Text, pack, unpack)
import           GHC.Generics             (Generic)
import qualified Network.MPD              as MPD
import           Network.Wreq
import           System.Console.Haskeline (InputT (), defaultSettings,
                                           getInputLine, outputStrLn, runInputT)

type Repl a = InputT IO a

data Track = Track
  { id        :: Int
  , stream_url :: Text
  } deriving (Generic, Show, Eq)

instance FromJSON Track

clientID :: Text
clientID = "f0b4138275ee2a59e4017654e9ff7c3f"

fetchTrack :: Text -> IO (Maybe Track)
fetchTrack url = do
  let opts = defaults & param "client_id" .~ [clientID]
                      & param "url" .~ [url]
                      & header "Accept" .~ ["application/json"]

  res <- asJSON =<< getWith opts "http://api.soundcloud.com/resolve"
  return $ res ^? responseBody

repl :: Repl ()
repl = do
  trackURL <- getInputLine "SoundCloud track URL:"
  case trackURL of
    Nothing -> outputStrLn "Goodbye."
    Just trackURL' -> do
      maybeTrack <- liftIO . fetchTrack $ pack trackURL'

      case maybeTrack of
        Nothing -> outputStrLn "The track you searched for couldn't be found"
        Just track -> do
          liftIO . MPD.withMPD $ do
            liftIO $ putStrLn "Starting track..."
            MPD.add . fromString . unpack $ stream_url track <> "?client_id=" <> clientID
            MPD.play Nothing
            MPD.currentSong

          return ()
      repl

main :: IO ()
main = runInputT defaultSettings repl

