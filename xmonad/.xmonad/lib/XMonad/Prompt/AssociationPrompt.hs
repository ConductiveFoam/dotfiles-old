module XMonad.Prompt.AssociationPrompt
  ( AssociationPrompt(..), associationPrompt
  ) where

-- Imports
import Data.Map (Map, (!?), keys)

import XMonad.Core (X, whenJust)

import XMonad.Prompt (XPrompt(showXPrompt), XPConfig, mkXPrompt, mkComplFunFromList')

-- AssociationPrompt
data AssociationPrompt = AssociationPrompt String
instance XPrompt AssociationPrompt where
  showXPrompt (AssociationPrompt name) = name

associationPrompt :: String -> (Map String a) -> (a -> X ()) -> XPConfig -> X ()
associationPrompt name map runner xpc = mkXPrompt (AssociationPrompt name) xpc (mkComplFunFromList' $ keys map) $ flip whenJust runner . (map !?)
