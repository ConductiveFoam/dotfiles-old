module XMonad.Prompt.ListCompletedPrompt
  ( ListCompletedPrompt, listCompletedPrompt
  ) where

-- Imports
import XMonad.Core (X)

import XMonad.Prompt (XPrompt(showXPrompt), XPConfig, mkXPrompt, mkComplFunFromList')

-- ListCompletedPrompt
data ListCompletedPrompt = ListCompletedPrompt String
instance XPrompt ListCompletedPrompt where
  showXPrompt (ListCompletedPrompt name) = name

listCompletedPrompt :: String -> [String] -> (String -> X ()) -> XPConfig -> X ()
listCompletedPrompt name compls runner xpc = mkXPrompt (ListCompletedPrompt name) xpc (mkComplFunFromList' compls) runner
