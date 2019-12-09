module MyBars
  ( mainScreenBar, loadBar, musicBar
  , primaryBar, secondaryBar
  ) where

-- Imports
import Data.Default
import XMobar.Config
import MyVisuals

mainScreenBar, secondaryScreenBar, loadBar, musicBar, primaryBar, secondaryBar :: Config
mainScreenBar = myDefaultXMobar
  { position = Static 1080 0 1844 19
  , wmName = "XMobar - Main"
  , commands = [ Run "StdinReader" [] []
                      ] ++ (fst infoTime)
  , template = " %StdinReader% }{ " ++ (snd infoTime) ++ " "
  }
secondaryScreenBar = myDefaultXMobar
  { screen = "1"
  , alpha = 220
  }
loadBar = secondaryScreenBar
  { position = Top
  , wmName = "XMobar - Load"
  , commands = (fst infoNet) ++ (fst infoMisc) ++ (fst infoLoad)
  , template = " " ++ (snd infoNet) ++ " | " ++ (snd infoMisc) ++ " }{ " ++ (snd infoLoad) ++ " "
  }
musicBar = secondaryScreenBar
  { position = Bottom
  , wmName = "XMobar - Music"
  , template = " " ++ infoMPD ++ "}{" ++ infoVolume ++ " "
  }

primaryBar = myDefaultXMobar
  { position = Static 0 0 1844 19
  , wmName = "XMobar - Primary"
  , commands = [ Run "StdinReader" [] []
                      ] ++ (fst infoMisc) ++ (fst infoTime)
  , template = " %StdinReader% }{ " ++ infoVolume ++ " | " ++ (snd infoMisc) ++ " | " ++ (snd infoTime) ++ " "
  }
secondaryBar = myDefaultXMobar
  { position = Bottom
  , wmName = "XMobar - Secondary"
  , commands = (fst infoNet) ++ (fst infoLoad)
  , template = (snd infoNet) ++ " | " ++ infoMPD ++ "}{" ++ (snd infoLoad) ++ " "
  }

-- Internals
myDefaultXMobar :: Config
myDefaultXMobar = def
  { font = myFont
  , bgColor = colBackground
  , fgColor = colForeground
  }

colConfig :: [String]
colConfig = ["-l", colDBlue, "-n", colDGreen, "-h", colDRed]

infoMisc, infoLoad, infoNet, infoTime :: ([Runnable], String)
infoMisc = ( [ Run "Kbd" ["[(\"us(dvorak)\", \"DV\"), (\"us\", \"US\"), (\"de\", \"DE\")]"] [] ]
           , "XSS %xmobar_xssmode.sh% | <fc=" ++ colDMagenta ++ ">%kbd%</fc>"
           )
infoLoad = ( [ Run "Cpu" ["10"] $ colConfig ++ ["-L", "15", "-H", "50"]
             , Run "Memory" ["10"] $ colConfig ++ ["-L", "15", "-H", "50", "-t", "Mem: <usedratio>"]
             , Run "Swap" ["10"] $ colConfig ++ ["-L", "15", "-H", "50"]
             ]
           , "%cpu% * Temp: %xmobar_coretemp.sh% | %memory%%xmobar_gpumem.sh% * %swap%"
           )
infoNet = ( [ Run "DynNetwork" ["10"] ["-t", "<dev>: <fc=" ++ colDBlue ++ "><rx></fc>;<fc=" ++ colDBlue ++ "><tx></fc>KB"] ]
          , "%dynnetwork%"
          )
infoTime = ( [ Run "Date" ["\"%l:%M %p\"", "\"time\"", "60"] []
             , Run "Uptime" ["[]", "60"] []
             ]
           , "<fc=" ++ colDMagenta ++ ">%xmobar_time.sh% %time%</fc> * %uptime%"
           )

infoMPD, infoVolume :: String
infoMPD = "%xmobar_mpdstatus.sh%"
infoVolume = "%xmobar_volume.sh%"
