{-# LANGUAGE
  DeriveDataTypeable
  #-}
module XMonad.Actions.LoggedAction
  ( withLog
  , logAction
  ) where
import XMonad.Config.Prime
  ( ExtensionClass(initialValue)
  , Typeable
  )
import XMonad (X, refresh)
import XMonad.Util.ExtensibleState (gets, put)
import XMonad.Util.Loggers (Logger)

newtype ActionState = ActionState { actionState :: Maybe String } deriving (Typeable, Read, Show)
instance ExtensionClass ActionState where
  initialValue = ActionState Nothing

withLog :: String -> X a -> X a
withLog name a = do
  (put . ActionState . Just) name
  refresh
  result <- a
  (put . ActionState) Nothing
  refresh
  return result

logAction :: Logger
logAction = gets actionState
