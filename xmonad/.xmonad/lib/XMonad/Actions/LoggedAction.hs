{-# LANGUAGE
  DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Actions.LoggedAction
  ( loggedAction
  , logAction
  ) where
import XMonad.Config.Prime
  ( ExtensionClass(initialValue), StateExtension
  , Typeable
  )
import XMonad (refresh)
import XMonad.Util.ExtensibleState (gets, put)
import XMonad.Util.Loggers (Logger)

newtype ActionState = ActionState { actionState :: Maybe String } deriving (Typeable, Read, Show)
instance ExtensionClass ActionState where
  initialValue = ActionState Nothing

loggedAction name sub = do
  (put . ActionState . Just) name
  refresh
  sub
  (put . ActionState) Nothing
  refresh

logAction :: Logger
logAction = do
  action <- gets actionState
  case action of
    Nothing -> return Nothing
    j -> return j
