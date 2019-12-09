module XMonad.Actions.MPD
  ( Status(..)
  , MPD.Song(..)
  , MPD.PlaybackState(..)
  , liftMPD, liftMPD_
  , state, toggle, play, pause, stop
  , next, previous
  , status, toggleStatus
  , consume, random, repeat, single
  , delete, clear
  , currentSong
  , showPlaylist, showSong
  , MPD.withMPD
  ) where
import Prelude hiding (repeat)

import Control.Monad (liftM)
import Data.List (intercalate)
import qualified Data.Map as M

import XMonad (MonadIO(liftIO))

import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as Cmd

-- Note: Network.MPD uses 0-indexing while mpd uses 1-indexing

data Status = Random | Repeat | Consume | Single
  deriving ( Show, Read, Eq )

liftMPD :: MonadIO m => MPD.MPD a -> m (MPD.Response a)
liftMPD = liftIO . MPD.withMPD

liftMPD_ :: MonadIO m => MPD.MPD a -> m ()
liftMPD_ = liftM (const ()) . liftMPD

status :: MonadIO m => Status -> m Bool
status Random = status'' MPD.stRandom
status Repeat = status'' MPD.stRandom
status Consume = status'' MPD.stRandom
status Single = status'' MPD.stRandom

state :: MonadIO m => m MPD.PlaybackState
state = status' MPD.Stopped MPD.stState

status' :: MonadIO m => a -> (MPD.Status -> a) -> m a
status' d g = (liftMPD MPD.status) >>= return . (either (const d) g)
status'' :: MonadIO m => (MPD.Status -> Bool) -> m Bool
status'' = status' False

toggleStatus :: MonadIO m => Status -> m ()
toggleStatus Random = toggleStatus' MPD.stRandom MPD.random
toggleStatus Repeat = toggleStatus' MPD.stRepeat MPD.repeat
toggleStatus Consume = toggleStatus' MPD.stConsume MPD.consume
toggleStatus Single = toggleStatus' MPD.stSingle MPD.single

toggleStatus' :: MonadIO m => (MPD.Status -> Bool) -> (Bool -> MPD.MPD ())  -> m ()
toggleStatus' g s = liftMPD_ $ do
  current <- MPD.status
  s $ not (g current)

play, delete :: MonadIO m => Int -> m ()
play = liftMPD_ . MPD.play . Just
delete = liftMPD_ . MPD.delete

random, single, consume, repeat, pause :: MonadIO m => Bool -> m ()
random = liftMPD_ . MPD.random
single = liftMPD_ . MPD.single
consume = liftMPD_ . MPD.consume
repeat = liftMPD_ . MPD.repeat
pause = liftMPD_ . MPD.pause
toggle, stop, next, previous, clear :: MonadIO m => m ()
toggle = liftMPD_ Cmd.toggle
stop = liftMPD_ MPD.stop
next = liftMPD_ MPD.next
previous = liftMPD_ MPD.previous
clear = liftMPD_ MPD.clear

currentSong :: MonadIO m => m (Maybe Int)
currentSong = status' Nothing MPD.stSongPos

showPlaylist :: MonadIO m => m String
showPlaylist = liftIO $ (MPD.withMPD $ MPD.playlistInfo Nothing) >>= return . (either show right)
  where
    right [] = "No playlist"
    right ls = unlines $ enumerate <$> ls
    enumerate song = (maybe "" ((++ ": ") . show . (+ 1)) $ MPD.sgIndex song) ++ (showSong' song)

showSong :: MonadIO m => Int -> m String
showSong i = liftIO $ do
    response <- MPD.withMPD . MPD.playlistInfo $ Just i
    return $ either show (showSong' . (!! 0)) response

showSong' :: MPD.Song -> String
showSong' MPD.Song { MPD.sgTags = tags' } =
    (showMeta MPD.Artist tags') ++ " -- " ++
    (showMeta MPD.Album tags') ++ " #" ++
    (showMeta MPD.Track tags') ++ " -- " ++
    (showMeta MPD.Title tags')
  where
    showValue = intercalate ", " . (MPD.toString <$>)
    showMeta meta tags = maybe "" showValue $ M.lookup meta tags
