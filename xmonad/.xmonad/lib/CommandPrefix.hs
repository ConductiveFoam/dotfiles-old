{-# LANGUAGE
  DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CommandPrefix
  ( CommandPrefix(..)
  , resetPrefix, modifyPrefix
  , prefixToString
  , prefixedAction, withPrefix
  , logPrefix
  ) where

-- Imports
import Control.Monad (replicateM_)

import XMonad (refresh)
import XMonad.Config.Prime
  ( ExtensionClass(initialValue), StateExtension
  , Typeable
  )
import XMonad.Hooks.DynamicLog (xmobarColor)
import XMonad.Util.Loggers (Logger)
import XMonad.Util.ExtensibleState (gets, modify, put)

-- CommandPrefix
newtype CommandPrefix = CommandPrefix { commandPrefix :: Int } deriving (Typeable, Read, Show)
instance ExtensionClass CommandPrefix where
  initialValue = CommandPrefix 0

resetPrefix = put (initialValue :: CommandPrefix)
modifyPrefix n = modify $ CommandPrefix . (\c -> c * 10 + n) . commandPrefix
prefixToString :: Int -> String
prefixToString 0 = ""
prefixToString n = show n

prefixedAction a = do
  prefix <- gets commandPrefix
  if prefix == 0 then
    a
    else
    (replicateM_ (prefix - 1) a) >> a
  resetPrefix
  refresh

withPrefix f = do
  prefix <- gets commandPrefix
  f prefix
  resetPrefix
  refresh

logPrefix :: (Int -> String) -> Logger
logPrefix fmt = do
  prefix <- gets commandPrefix
  return . Just $ fmt prefix
