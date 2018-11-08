{-# LANGUAGE
  DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CommandPrefix(
  CommandPrefix(..),
  resetPrefix, modifyPrefix,
  prefixToString,
  prefixedAction, spawnWithPrefix,
  logPrefix) where

import Control.Monad

import MyColors

import XMonad
import XMonad.Config.Prime(ExtensionClass,StateExtension(..))
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import qualified XMonad.Util.ExtensibleState as XS

newtype CommandPrefix = CommandPrefix { commandPrefix :: Int } deriving (Typeable, Read, Show)
instance ExtensionClass CommandPrefix where
  initialValue = CommandPrefix 0
  extensionType = StateExtension

resetPrefix = XS.put $ CommandPrefix 0
modifyPrefix n = XS.modify $ CommandPrefix . (\c -> c * 10 + n) . commandPrefix
prefixToString :: Int -> String
prefixToString 0 = ""
prefixToString n = show n

prefixedAction a = do
  prefix <- XS.gets commandPrefix
  if prefix == 0 then
    a
    else
    (replicateM_ (prefix - 1) a) >> a
  resetPrefix
  refresh
spawnWithPrefix cmd = do
  prefix <- XS.gets commandPrefix
  spawn $ cmd ++ " " ++ (prefixToString prefix)
  resetPrefix
  refresh
logPrefix :: Logger
logPrefix = do
  prefix <- XS.gets commandPrefix
  return . Just $ xmobarColor colDYellow "" $ prefixToString prefix

