module XMonad.Prompt.UdiskiePrompt
  ( mountDevicePrompt, unmountDevicePrompt
  , listDevices
  , udiskiePrompt
  ) where

import Data.Char (isAlpha, isDigit)
import Data.List (sort)
import System.Directory (listDirectory, getHomeDirectory, getCurrentDirectory)
import System.FilePath (isPathSeparator, (</>))

import XMonad

import XMonad.Prompt (XPConfig)
import XMonad.Prompt.ListCompletedPrompt (listCompletedPrompt)
import XMonad.Util.Run (runProcessWithInput)

udiskiePrompt :: String -> String -> FilePath -> [String] -> XPConfig -> X ()
udiskiePrompt name action from list xpc = do
  from' <- resolvePath from
  listCompletedPrompt name list (\w -> spawn $ "udiskie-" ++ action ++ " \"" ++ (from' </> w) ++ "\"") xpc

mountDevicePrompt, unmountDevicePrompt :: FilePath -> XPConfig -> X ()
mountDevicePrompt from xpc = do
  devices <- runProcessWithInput "udiskie_mountable_devices.sh" [] ""
  let devices' = (filter notBoring . -- Only show certain devices & partitions
                  (fmap $ drop $ (length from) + 1) . -- Remove path
                  lines
                 ) devices
  udiskiePrompt "Mount device: " "mount -r" from (sort devices') xpc

unmountDevicePrompt from xpc = do
  devices <- listDevices from
  udiskiePrompt "Unmount device: " "umount -f" from (sort devices) xpc

listDevices :: MonadIO m => FilePath -> m [String]
listDevices from = io $ (resolvePath from) >>= listDirectory

notBoring :: String -> Bool
notBoring ('s':'d':x:n) = (isAlpha x) && (0 < length n) && (all isDigit n)
notBoring ('s':'r':[n]) = isDigit n
notBoring _ = False

resolvePath :: MonadIO m => FilePath -> m FilePath
resolvePath p@(h:_) | isPathSeparator h = return p
resolvePath ('~':h:p) = do
  let p' = if isPathSeparator h then p else h:p
  home <- io $ getHomeDirectory
  return $ home </> p'
resolvePath p = do
  curr <- io $ getCurrentDirectory
  return $ curr </> p
