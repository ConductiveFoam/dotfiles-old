{-# LANGUAGE
  DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CommandPrefix
  ( CommandPrefix(..)
  , resetPrefix, modifyPrefix, prependToPrefix
  , prefixToString
  , prefixedAction, withPrefix
  , logPrefix
  ) where

-- Imports
import Control.Monad (replicateM_)

import XMonad (refresh)
import XMonad.Config.Prime
  ( ExtensionClass(initialValue)
  , Typeable
  , X
  )
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers (Logger)
import XMonad.Util.PureX (XLike(toX))

-- CommandPrefix
newtype CommandPrefix = CommandPrefix { commandPrefix :: Int } deriving (Typeable, Read, Show)
instance ExtensionClass CommandPrefix where
  initialValue = CommandPrefix 0

getPrefix :: XLike m => m Int
getPrefix = XS.gets commandPrefix

modifyPrefix :: XLike m => (Int -> Int) -> m ()
modifyPrefix f = XS.modify $ CommandPrefix . f . commandPrefix

prependToPrefix :: XLike m => Int -> m ()
prependToPrefix n = modifyPrefix (\c -> c * 10 + n)

resetPrefix :: XLike m => m ()
resetPrefix = XS.put (initialValue :: CommandPrefix)

prefixToString :: Int -> String
prefixToString 0 = ""
prefixToString n = show n

prefixedAction :: X a -> X ()
prefixedAction a = do
  prefix <- getPrefix
  if prefix == 0 then
    a
    else
    (replicateM_ (prefix - 1) a) >> a
  resetPrefix
  refresh

withPrefix :: (Int -> X a) -> X a
withPrefix f = do
  prefix <- getPrefix
  a <- f prefix
  resetPrefix
  refresh
  return a

logPrefix :: (Int -> String) -> Logger
logPrefix fmt = do
  prefix <- getPrefix
  return . Just $ fmt prefix
