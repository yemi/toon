{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens             ((&), (.~), (^?))
import           Control.Monad            (forM_, forever)
import           Control.Monad.Free       (Free, foldFree)
import           Control.Monad.Free.Class (liftF)
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson               (FromJSON, parseJSON, withObject,
                                           (.:))
import           Data.Monoid              ((<>))
import           Data.String              (fromString)
import           Data.Text                (Text, pack, unpack)
import           Network.MPD              (Response, Song, State (..), add,
                                           clear, next, pause, play, playlistId,
                                           previous, stState, status, withMPD)
import           Network.Wreq             (asJSON, defaults, getWith, header,
                                           param, responseBody)
import           System.Console.Haskeline (InputT (), defaultSettings,
                                           getInputChar,
                                           getInputLineWithInitial, outputStrLn,
                                           outputStr, runInputT)

type UI a = InputT IO a

data Resource
  = Track { t_id :: Int, t_title :: Text, t_streamURL :: Text }
  | Playlist { pl_id :: Int, pl_title :: Text, pl_tracks :: [Resource]}

type PlayerState = State

instance FromJSON Resource where
  parseJSON = withObject "track, playlist or user" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "track" -> Track <$> o .: "id" <*> o .: "title" <*> o .: "stream_url"
      "playlist" -> Playlist <$> o .: "id" <*> o .: "title" <*> o .: "tracks"
      _ -> fail ("unknown kind: " ++ kind)

clientID :: Text
clientID = "f0b4138275ee2a59e4017654e9ff7c3f"

fetchResource :: Text -> IO (Maybe Resource)
fetchResource url = do
  let opts = defaults & param "client_id" .~ [clientID]
                      & param "url" .~ [url]
                      & header "Accept" .~ ["application/json"]
  res <- asJSON =<< getWith opts "http://api.soundcloud.com/resolve"
  return $ res ^? responseBody

fetchRelatedTracks :: Resource -> IO (Maybe [Resource])
fetchRelatedTracks Track { t_id } = do
  let opts = defaults & param "client_id" .~ [clientID]
                      & header "Accept" .~ ["application/json"]
  res <- asJSON =<< getWith opts ("http://api.soundcloud.com/tracks/" <> show t_id <> "/related")
  return $ res ^? responseBody

addTrackToPlaylist :: Resource -> IO (Response ())
addTrackToPlaylist Track { t_streamURL } =
  withMPD . add . fromString . unpack $ t_streamURL <> "?client_id=" <> clientID

addTracksToPlaylist :: [Resource] -> IO ()
addTracksToPlaylist tracks = forM_ tracks addTrackToPlaylist

getSongsInPlaylist :: IO [Song]
getSongsInPlaylist = do
  res <- withMPD $ playlistId Nothing
  return $ either (const mempty) id res

searchResource :: Maybe Text -> IO (Maybe Resource)
searchResource maybeURL = case maybeURL of
  Nothing -> return Nothing
  Just url -> fetchResource url

handleResource :: Maybe Resource -> IO (Maybe [Resource])
handleResource maybeResource = case maybeResource of
  Nothing -> return Nothing
  Just resource -> case resource of
    track@Track {} -> do
      withMPD clear
      addTrackToPlaylist track
      fetchRelatedTracks track
    _ -> return Nothing

handleResources :: Maybe [Resource] -> IO PlayerState
handleResources maybeResources = case maybeResources of
  Nothing -> return Stopped
  Just resources -> case resources of
    Track {}:_ -> do
      putStrLn "Found tracks!"
      addTracksToPlaylist resources
      putStrLn "Starting playlist :)"
      withMPD $ play Nothing
      return Playing

renderUI :: PlayerState -> UI ()
renderUI Stopped = do
  outputStrLn "URL to track:"
  maybeURL <- getInputLineWithInitial "> " ("https://soundcloud.com/", "")
  maybeResource <- liftIO . searchResource $ pack <$> maybeURL
  maybeResources <- liftIO $ handleResource maybeResource
  playerState' <- liftIO $ handleResources maybeResources
  renderUI playerState'
renderUI _ = do
  maybeChar <- getInputChar "> "
  case maybeChar of
    Nothing -> return ()
    Just char -> case char of
      'h' -> liftIO (withMPD previous) >> return ()
      'j' -> liftIO (withMPD (pause True)) >> return ()
      'k' -> liftIO (withMPD (play Nothing)) >> return ()
      'l' -> liftIO (withMPD next) >> return ()
      'r' -> liftIO (withMPD (pause True)) >> renderUI Stopped
      _ -> return ()

  eitherStatus <- liftIO $ withMPD status
  renderUI $ either (const Stopped) stState eitherStatus

main :: IO ()
main = runInputT defaultSettings $ do
  outputStrLn "\ntt                          "
  outputStrLn "tt     oooo   oooo  nn nnn  "
  outputStrLn "tttt  oo  oo oo  oo nnn  nn "
  outputStrLn "tt    oo  oo oo  oo nn   nn "
  outputStrLn " tttt  oooo   oooo  nn   nn\n"
  outputStrLn "The SoundCloud based interactive radio station\n"
  outputStrLn "Playback commands (vim style):\n"
  outputStrLn "h - previous"
  outputStrLn "j - pause"
  outputStrLn "k - play"
  outputStrLn "l - next\n"
  outputStrLn "Tooning:\n"
  outputStrLn "r - enter URL to track\n"

  renderUI Stopped

