module XMonad.Actions.MPD
  ( Toggleable(..)
  , liftMPD, liftMPD_
  , invert, toggle
  , play, pause, stop
  , next, previous
  , delete, clear
  , showPlaylists
  , MPD.withMPD
  ) where

import Control.Monad (liftM)
import Data.List (intercalate)
import qualified Data.Map as M

import XMonad (MonadIO(liftIO))

import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as Cmd

data Toggleable = Random | Repeat | Consume | Single
  deriving ( Show, Read, Eq )

liftMPD :: MonadIO m => MPD.MPD a -> m (MPD.Response a)
liftMPD = liftIO . MPD.withMPD

liftMPD_ :: MonadIO m => MPD.MPD a -> m ()
liftMPD_ = liftM (const ()) . liftMPD

invert :: MonadIO m => Toggleable -> m ()
invert Random = liftMPD_ $ invert' MPD.stRandom MPD.random
invert Repeat = liftMPD_ $ invert' MPD.stRepeat MPD.repeat
invert Consume = liftMPD_ $ invert' MPD.stConsume MPD.consume
invert Single = liftMPD_ $ invert' MPD.stSingle MPD.single

invert' :: MPD.MonadMPD m => (MPD.Status -> Bool) -> (Bool -> m ())  -> m ()
invert' g s = do
  status <- MPD.status
  s $ not (g status)

toggle :: MonadIO m => m ()
toggle = liftMPD_ Cmd.toggle

play, delete :: MonadIO m => Int -> m ()
play = liftMPD_ . MPD.play . Just
delete = liftMPD_ . MPD.delete

pause :: MonadIO m => Bool -> m ()
pause = liftMPD_ . MPD.pause
stop, next, previous, clear :: MonadIO m => m ()
stop = liftMPD_ MPD.stop
next = liftMPD_ MPD.next
previous = liftMPD_ MPD.previous
clear = liftMPD_ MPD.clear

showPlaylists :: MonadIO m => m String
showPlaylists = liftIO $ do
  response <- MPD.withMPD $ MPD.playlistInfo Nothing
  case response of
    Left e -> return $ show e
    Right is -> return . unlines $ enumerate <$> is
      where
        showValue = intercalate ", " . (MPD.toString <$>)
        showMeta meta tags = maybe "" showValue $ M.lookup meta tags
        enumerate MPD.Song { MPD.sgTags = tags, MPD.sgIndex = index} =
          (maybe "" ((++ ": ") . show) index) ++
          (showMeta MPD.Artist tags) ++ " -- " ++
          (showMeta MPD.Album tags) ++ " #" ++
          (showMeta MPD.Track tags) ++ " -- " ++
          (showMeta MPD.Title tags)
