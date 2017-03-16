{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Lens
import           Control.Monad            (forM_, forever, void)
import           Control.Monad.Free       (Free, foldFree)
import           Control.Monad.Free.Class (liftF)
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson               (FromJSON, parseJSON, withObject,
                                           (.:))
import           Data.Monoid              ((<>))
import Data.Maybe (fromMaybe)
import           Data.String              (fromString)
import           Data.Text                (Text, pack, unpack)
import           Network.MPD              (Response, Song, State (..), add,
                                           clear, next, pause, play, playlistId,
                                           previous, stState, status, withMPD)
import           Network.Wreq             (asJSON, defaults, getWith, header,
                                           param, responseBody)
import           System.Console.Haskeline (InputT (), defaultSettings,
                                           getInputChar,
                                           getInputLineWithInitial, outputStr,
                                           outputStrLn, runInputT)
import           System.Process           (createProcess, proc)

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Data.Vector as V
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

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

-- searchResource :: Maybe Text -> IO (Maybe Resource)
-- searchResource maybeURL = case maybeURL of
--   Nothing -> return Nothing
--   Just url -> fetchResource url

handleResource :: Resource -> IO (Maybe [Resource])
handleResource resource = case resource of
  track@Track {} -> do
    withMPD clear
    addTrackToPlaylist track
    -- fetchRelatedTracks track
    withMPD $ play Nothing
    return Nothing
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

-- renderUI :: PlayerState -> UI ()
-- renderUI Stopped = do
--   outputStrLn "URL to track:"
--   maybeURL <- getInputLineWithInitial "> " ("https://soundcloud.com/", "")
--   maybeResource <- liftIO . searchResource $ pack <$> maybeURL
--   maybeResources <- liftIO $ handleResource maybeResource
--   playerState' <- liftIO $ handleResources maybeResources
--   renderUI playerState'
-- renderUI _ = do
--   maybeChar <- getInputChar "> "
--   case maybeChar of
--     Nothing -> return ()
--     Just char -> case char of
--       'h' -> liftIO (withMPD previous) >> return ()
--       'j' -> liftIO (withMPD (pause True)) >> return ()
--       'k' -> liftIO (withMPD (play Nothing)) >> return ()
--       'l' -> liftIO (withMPD next) >> return ()
--       'r' -> liftIO (withMPD (pause True)) >> renderUI Stopped
--       _ -> return ()

--   eitherStatus <- liftIO $ withMPD status
--   renderUI $ either (const Stopped) stState eitherStatus

-- mainBackup :: IO ()
-- mainBackup = runInputT defaultSettings $ do

--   -- Start `mpd` server
--   _ <- liftIO . createProcess $ proc "mpd" ["--no-config"]


--   -- Usage info
--   outputStrLn "\ntt                          "
--   outputStrLn "tt     oooo   oooo  nn nnn  "
--   outputStrLn "tttt  oo  oo oo  oo nnn  nn "
--   outputStrLn "tt    oo  oo oo  oo nn   nn "
--   outputStrLn " tttt  oooo   oooo  nn   nn\n"
--   outputStrLn "The SoundCloud based interactive radio station\n"
--   outputStrLn "Playback commands (vim style):\n"
--   outputStrLn "h - previous"
--   outputStrLn "j - pause"
--   outputStrLn "k - play"
--   outputStrLn "l - next\n"
--   outputStrLn "Tooning:\n"
--   outputStrLn "r - enter URL to track\n"

--   -- Render UI
--   renderUI Stopped




data AppState = AppState
  { _trackUrlEditor :: E.Editor
  , _resource :: Maybe Resource
  }

makeLenses ''AppState

-- drawUI :: (Show a) => L.List a -> [Widget]
-- drawUI l = [ui]
--     where
--         label = str "Item " <+> cur <+> str " of " <+> total
--         cur = case l^.(L.listSelectedL) of
--                 Nothing -> str "-"
--                 Just i -> str (show (i + 1))
--         total = str $ show $ V.length $ l^.(L.listElementsL)
--         box = B.borderWithLabel label $
--               hLimit 25 $
--               vLimit 15 $
--               L.renderList l listDrawElement
--         ui = C.vCenter $ vBox [ C.hCenter box
--                               , str " "
--                               , E.renderEditor $ 
--                               , C.hCenter $ str "Press +/- to add/remove list elements."
--                               , C.hCenter $ str "Press Esc to exit."
--                               ]
currentEditorL :: AppState -> Lens' AppState E.Editor
currentEditorL st = trackUrlEditor

drawUI :: AppState -> [T.Widget]
drawUI st = [ui]
  where
    ui = C.center $
      (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 $ E.renderEditor $ st^.trackUrlEditor)) <=>
      (str resourceTitle)

    resourceTitle = case _resource st of
      Nothing -> "no resource has been fetched"
      Just resource -> unpack $ t_title resource

-- appEvent :: L.List Char -> V.Event -> T.EventM (T.Next (L.List Char))
-- appEvent l e =
--     case e of
--         V.EvKey (V.KChar '+') [] ->
--             let el = nextElement (L.listElements l)
--                 pos = V.length $ l^.(L.listElementsL)
--             in M.continue $ L.listInsert pos el l

--         V.EvKey (V.KChar '-') [] ->
--             case l^.(L.listSelectedL) of
--                 Nothing -> M.continue l
--                 Just i -> M.continue $ L.listRemove i l

--         V.EvKey V.KEsc [] -> M.halt l

--         ev -> M.continue =<< T.handleEvent ev l
--     where
--       nextElement :: V.Vector Char -> Char
--       nextElement v = fromMaybe '?' $ V.find (flip V.notElem v) (V.fromList ['a' .. 'z'])

appEvent :: AppState -> V.Event -> T.EventM (T.Next AppState)
appEvent st ev = case ev of
  V.EvKey V.KEsc [] -> M.halt st
  V.EvKey V.KEnter [] -> do
    let url = pack . unlines . E.getEditContents $ st^.trackUrlEditor
    resource <- liftIO $ fetchResource url
    case resource of
      Nothing -> M.continue st
      Just resource' -> do
        liftIO $ handleResource resource'
        M.continue $ st{ _resource = Just resource' }
  _ -> M.continue =<< T.handleEventLensed st (currentEditorL st) ev

listDrawElement :: (Show a) => Bool -> a -> Widget
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

-- initialState :: L.List Char
-- initialState = L.list (T.Name "list") (V.fromList ['a','b','c']) 1

initialState :: AppState
initialState = AppState { _trackUrlEditor = E.editor "trackUrl" (str . unlines) Nothing ""
                        , _resource = Nothing
                        }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.white `on` V.blue)
  , (L.listSelectedAttr,    V.blue `on` V.white)
  , (customAttr,            fg V.cyan)
  ]

theApp :: M.App AppState V.Event
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appLiftVtyEvent = id
        }

main :: IO ()
main = do
  _ <- liftIO . createProcess $ proc "mpd" ["--no-config"]
  void $ M.defaultMain theApp initialState
