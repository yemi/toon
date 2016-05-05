{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens             ((&), (.~), (^?))
import           Control.Monad            (forM_)
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson               (FromJSON, Value (..), parseJSON,
                                           (.:))
import           Data.Monoid              ((<>))
import           Data.String              (fromString)
import           Data.Text                (Text, pack, unpack)
import           Network.MPD              hiding (Track)
import           Network.Wreq
import           System.Console.Haskeline (InputT (), defaultSettings,
                                           getInputLine, outputStrLn, runInputT)

type Repl a = InputT IO a

data Track = Track
  { t_id        :: Int
  , t_streamURL :: Text
  } deriving (Show, Eq)

instance FromJSON Track where
  parseJSON (Object v) = Track <$> v .: "id" <*> v .: "stream_url"
  parseJSON _ = mempty

clientID :: Text
clientID = "f0b4138275ee2a59e4017654e9ff7c3f"

fetchTrack :: Text -> IO (Maybe Track)
fetchTrack url = do
  let opts = defaults & param "client_id" .~ [clientID]
                      & param "url" .~ [url]
                      & header "Accept" .~ ["application/json"]
  res <- asJSON =<< getWith opts "http://api.soundcloud.com/resolve"
  return $ res ^? responseBody

fetchRelatedTracks :: Track -> IO (Maybe [Track])
fetchRelatedTracks Track { t_id } = do
  let opts = defaults & param "client_id" .~ [clientID]
                      & header "Accept" .~ ["application/json"]
  res <- asJSON =<< getWith opts ("http://api.soundcloud.com/tracks/" <> show t_id <> "/related")
  return $ res ^? responseBody

addTracksToPlaylist :: [Track] -> IO ()
addTracksToPlaylist tracks = do
  forM_ tracks $ \Track { t_streamURL } ->
    withMPD $ add . fromString . unpack $ t_streamURL <> "?client_id=" <> clientID
  return ()

getSongsInPlaylist :: IO [Song]
getSongsInPlaylist = do
  res <- withMPD $ playlistId Nothing
  return $ either (const mempty) id res

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
          maybeRelatedTracks <- liftIO $ fetchRelatedTracks track

          case maybeRelatedTracks of
            Nothing -> outputStrLn "The track doesnt have any related tracks"
            Just relatedTracks -> do
              outputStrLn "Found tracks!"
              liftIO $ addTracksToPlaylist relatedTracks

              songs <- liftIO getSongsInPlaylist

              outputStrLn "Starting playlist :)"
              liftIO . withMPD $ play Nothing

              return ()

          return ()
      repl

main :: IO ()
main = runInputT defaultSettings repl

