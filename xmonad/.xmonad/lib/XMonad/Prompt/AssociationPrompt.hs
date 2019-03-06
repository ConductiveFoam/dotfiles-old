module XMonad.Prompt.AssociationPrompt where

import qualified Data.Map as M

import XMonad.Core(X)

import XMonad.Prompt(XPrompt(..), XPConfig, mkXPrompt, mkComplFunFromList)

data AssociationPrompt = AssociationPrompt String
instance XPrompt AssociationPrompt where
  showXPrompt (AssociationPrompt name) = name

associationPrompt :: String -> (M.Map String String) -> (String -> X ()) -> XPConfig -> X ()
associationPrompt name map runner xpc = mkXPrompt (AssociationPrompt name) xpc (mkComplFunFromList $ M.keys map) runner'
  where runner' k = case (M.lookup k map) of
          Just a -> runner a
          Nothing -> return ()
