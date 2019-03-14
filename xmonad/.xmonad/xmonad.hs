-- Base
import Data.List(isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import qualified Data.Map as M
import System.IO
import System.Exit

-- XMonad base
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

-- Layouts
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Accordion
import XMonad.Layout.Column
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing

-- Prompt
import XMonad.Prompt(XPConfig(..), XPPosition(Top))
import XMonad.Prompt.Shell(shellPrompt)
import XMonad.Prompt.AssociationPrompt(associationPrompt)
import XMonad.Prompt.ListCompletedPrompt(listCompletedPrompt)

-- Hotkey config
import XMonad.Actions.Submap
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86

-- Actions
import XMonad.Operations
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe, safeSpawn, runInTerm)
import XMonad.Util.Paste(pasteSelection)

-- Custom
import MyColors
import CommandPrefix

-- Workspaces
wsMain = "main"
wsDev = "dev"
wsRead = "read"
wsGame = "game"
wsMsg = "msg"
wsMisc = "misc"
myWorkspaces = [ wsMain, wsDev, wsRead, wsGame, wsMsg, wsMisc ]

-- Mask shortnames
myModMask = mod4Mask
maskS = myModMask .|. shiftMask
maskC = myModMask .|. controlMask
maskCS = myModMask .|. shiftMask .|. controlMask

-- Terminal, commands, Prompts & dmenu prompts
myTerminal = "alacritty"
knownTerminalClasses = ["Alacritty", "Xfce4-terminal"] -- X window classes of terminal emulators
dmenuMpcLoadPlaylist = "mpc lsplaylists | dmenu | mpc load"
trayCmd = "if [[ ! $(pgrep stalonetray) ]]; then stalonetray -bg '" ++ colBackground ++ "'; fi"
makeScreenshotCmd opts name = "maim " ++ opts ++ " $HOME/screenshots/screenshot" ++ name ++"-$(date +%Y%m%d-%I-%M-%S).png"

promptSysUnits = ["redshiftd.service", "xss-deactivate.timer", "dunst.service"]
spawnSysctl cmd unit = spawn $ "sysdctl.sh " ++ cmd ++ " " ++ unit
sysctlPrompt name cmd xpc = listCompletedPrompt name promptSysUnits (spawnSysctl cmd) xpc
promptApps = ["firefox", "steam", "alacritty", "telegram-desktop", "teamspeak3", "vlc", "pavucontrol-qt", "libreoffice"]
spawnSteam = ("steam steam://run/" ++)
promptGames = M.fromList $
  [ ("Left 4 Dead 2", "left4gore -2")
  , ("Stellaris", spawnSteam "281990")
  , ("Full Bore", spawnSteam "264060")
  , ("Zen Bound 2",spawnSteam "61600")
  , ("Portal", spawnSteam "400")
  , ("Portal 2", spawnSteam "620")
  , ("Hollow Knight", "$HOME/games/Hollow\\ Knight/start.sh")
  , ("They Bleed Pixels", spawnSteam "211260")
  , ("Rex Rocket", spawnSteam "288020")
  , ("Dustforce", spawnSteam "65300")]
xpc = def { font = "xft:Fira Code:style=Bold:size=9:antialias=true"
          , bgColor = colBackground
          , fgColor = colForeground
          , bgHLight = colForeground
          , fgHLight = colBackground
          , promptBorderWidth = 0
          , position = Top
          }

-- Layout hook
myLayout = onWorkspace wsDev col $ onWorkspace wsRead read $
           onWorkspace wsGame (noBorders Full) $ onWorkspace wsMsg msg $
           onWorkspace wsMisc Accordion $ tiled ||| Mirror tiled
  where
    col = Column 1
    read = Mirror $ (Tall 2 delta (1/3)) ||| (mosaic 2 [2, 1])
    msg = col ||| Mirror col
    tiled = Tall 1 delta (1/2)

    delta = 3/100

-- Manage hook
myManageHook = composeOne $
  [ className =? terminal -?> doShift wsDev | terminal <- knownTerminalClasses ] ++ -- Move all terminal emulators to dev workspace
  [ (className =? "Xmessage" <&&> title =? "XMonad key binds") -?> doRectFloat (W.RationalRect 0.3 0 0.4 1)
  , className =? "Xmessage" -?> doRectFloat centerRect

  -- Gaming related
  , (className =? "Steam" <&&> title =? "Steam" ) -?> doShift wsGame
  , (className =? "Steam" <&&> title ..=? "Friends List") -?> doShift wsMsg
  , (className =? "Steam" <&&> title ..=? "Steam - News") -?> doHideIgnore
  , className =? "Steam" -?> doShift wsGame
  , className =? "Dustforce.bin.x86_64" -?> doFullFloat
  , className =? "hollow_knight.x86_64" -?> doShift wsGame

  -- Messenger & Communication
  , className =? "TelegramDesktop" -?> doShift wsMsg
  , className =? "TeamSpeak 3" -?> doShift wsMsg

  -- Miscellaneous
  , className =? "vlc" -?> doShift wsMisc
  , className =? "Thunderbird" -?> doShift wsMisc
  , className =? "Matplotlib" -?> doCenterFloat

  , isDialog -?> doCenterFloat
  , isFullscreen -?> doFullFloat
  , transience
  ]

-- Key bindings
-- %% Modifier is windows (mod4) key
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = myModMask}) = M.fromList $
  -- %% ! Launching and Killing programs
  [ ((maskS, xK_Return), spawn $ terminal conf) -- %! Launch terminal
  , ((myModMask, xK_Return), runInTerm "-t \"Terminal\"" "tmux a -t dev") -- %! Launch dev terminal
  , ((maskC, xK_Return), runInTerm "-t \"Terminal - Web\"" "tmux a -t web") -- %! Launch elinks terminal
  , ((maskS, xK_r), shellPrompt xpc) -- %! Launch app
  , ((myModMask, xK_r), listCompletedPrompt "Launch: " promptApps spawn xpc) -- %! Launch curated dmenu
  , ((myModMask, xK_g), associationPrompt "Start Game: " promptGames spawn xpc) -- %! Launch game
  , ((maskS, xK_c), kill) -- %! Close the focused window

  , ((myModMask, xK_space), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
  , ((maskS, xK_space), sendMessage FirstLayout) -- %!  Reset the layouts on the current workspace to default

  , ((myModMask, xK_n), refresh) -- %! Resize viewed windows to the correct size

  -- %% ! Moving focus
  , ((myModMask, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
  , ((maskS, xK_Tab), windows W.focusUp) -- %! Move focus to the previous window
  , ((myModMask, xK_j), windows W.focusDown) -- %! Move focus to the next window
  , ((myModMask, xK_k), windows W.focusUp) -- %! Move focus to the previous window
  , ((myModMask, xK_m), windows W.focusMaster) -- %! Move focus to the master window

  -- %% ! Modifying the window order
  , ((maskCS, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
  , ((maskS, xK_j), windows W.swapDown) -- %! Swap the focused window with the next window
  , ((maskS, xK_k), windows W.swapUp) -- %! Swap the focused window with the previous window

  -- %% ! Resizing the master/slave ratio
  , ((myModMask, xK_h), sendMessage Shrink) -- %! Shrink the master area
  , ((myModMask, xK_l), sendMessage Expand) -- %! Expand the master area

  -- %% ! Floating layer support
  , ((myModMask, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling
  , ((maskS, xK_f), withFocused $ windows . (rectFloat maximizeRect)) -- %! Float & maximize current window
  , ((maskC, xK_f), withFocused $ float) -- %! Float current window
  , ((myModMask, xK_f), withFocused $ windows . (rectFloat centerRect)) -- %! Float & center current window

  -- %% ! Increase or Decrease number of windows in the master area
  , ((myModMask, xK_comma), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((myModMask, xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip (myWorkspaces) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  -- mod-{w,e} %! Switch to physical/Xinerama screens 1, 2
  -- mod-shift-{w,e} %! Move client to screen 1, 2
  [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e] [0..] --, xK_r
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  [ ((maskC, xK_g), resetPrefix >> refresh) ] ++ -- %! Reset prefix
  -- mod-control-[0..9] %! Extend prefix
  [((maskC, key), (modifyPrefix $ (fromIntegral key) - 48) >> refresh) | key <- [xK_0 .. xK_9]]
  ++

  -- %% ! MPD & Audio control
  [ ((maskS, xK_m), spawn dmenuMpcLoadPlaylist) -- %! Load playlist
  , ((maskC, xK_m), submap . M.fromList $ -- %! Audio Submap:
    [ ((0, xK_p), spawnWithPrefix (\p -> "mpc play " ++ (prefixToString p))) -- %! MPC play $prefix
    , ((0, xK_n), spawn "mpc pause") -- %! MPC pause
    , ((shiftMask, xK_t), spawn "mpc stop") -- %! MPC stop
    , ((0, xK_t), spawn "mpc toggle") -- %! MPC toggle
    , ((0, xK_f), spawn "mpc next") -- %! MPC next
    , ((0, xK_b), spawn "mpc prev") -- %! MPC previous

    , ((0, xK_s), spawn "mpc random") -- %! MPC random
    , ((0, xK_r), spawn "mpc repeat") -- %! MPC repeat
    , ((0, xK_c), spawn "mpc consume") -- %! MPC consume
    , ((shiftMask, xK_s), spawn "mpc single") -- %! MPC single

    , ((0, xK_Delete), spawnWithPrefix (\p -> "mpc del " ++ (prefixToString p))) -- %! MPC del $prefix
    , ((controlMask, xK_Delete), spawn "mpc clear") -- %! MPC clear
    ])
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+") -- %! Increase volume
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-") -- %! Decrease volume
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle") -- %! Toggle mute
  , ((0, xF86XK_AudioPlay), spawn "mpc toggle") -- %! MPC toggle

  -- %% ! Systemctl integration
  , ((maskC, xK_s), submap . M.fromList $ -- %! Systemctl submap:
    [ ((0, xK_s), sysctlPrompt "Unit Status: " "status" xpc) -- %! Show unit status
    , ((0, xK_t), sysctlPrompt "Toggle Unit: " "toggle" xpc) -- %! Toggle unit
    , ((0, xK_a), sysctlPrompt "Start Unit: " "start" xpc) -- %! Start unit
    , ((0, xK_d), sysctlPrompt "Stop Unit: " "stop" xpc) -- %! Stop unit
    , ((0, xK_r), sysctlPrompt "Restart Unit: " "restart" xpc) -- %! Restart unit
    ])

  -- %% ! Quit xmonad, Power control
  , ((maskS, xK_q), io (exitWith ExitSuccess)) -- %! Quit xmonad
  , ((myModMask, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
  , ((maskCS, xK_q), spawn "systemctl poweroff") -- %! Shut off system
  , ((myModMask, xK_z), spawn "xscreensaver-command --lock") -- %! Lock screen
  , ((maskC, xK_z), spawn "togglexss.sh") -- %! Toggle automatic lock

  -- %% ! Notification control
  -- mod-ctrl-n %! Close notification
  -- mod-ctrl-shift-n %! Close all notifications
  -- mod-shift-h %! Show previous notification(s)

  -- %% ! Miscellaneous
  , ((maskC, xK_space), spawn "cyclexlayout.sh") -- %! Cycle keyboard layouts

  , ((maskC, xK_h), helpCommand) -- %! XMessage window with key binds

  , ((0, xK_Insert), pasteSelection) -- %! Paste selection

  , ((controlMask, xK_Print), spawn $ "sleep 0.2; " ++ (makeScreenshotCmd "-s -c 0.42,0.44,0.77 -b 2" "-selection")) -- %! Select window or rectangle, make screenshot of it
  , ((shiftMask, xK_Print), withFocused $ \w -> spawn $ makeScreenshotCmd ("--window " ++ (show w)) "-window") -- %! Make screenshot of focused window
  , ((0, xK_Print), spawn $ makeScreenshotCmd "" "") -- %! Make screenshot of whole desktop

  ]
  where
    helpCommand :: X ()
    helpCommand = spawn $ "awk -f $HOME/.xmonad/genhelp.awk $HOME/.xmonad/xmonad.hs | " ++
      "xmessage -buttons '' -title \"XMonad key binds\" -file -"

-- Main config
main = do
  spawn $ "nitrogen --restore"
  spawn trayCmd
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh $ desktopConfig
    { manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = desktopLayoutModifiers $ myLayout
    , logHook = dynamicLogWithPP def
      { ppCurrent = xmobarColor colDBlue "" . wrap "[" "]"
      , ppVisible = wrap "(" ")"
      , ppUrgent = xmobarColor colDRed colDYellow
      , ppTitle = xmobarColor colDMagenta "" . shorten 70

      , ppSep = " | "
      , ppExtras = [logPrefix]
      , ppOrder = \(ws:_:t:ex) -> ex ++ [ws,t]
      , ppOutput = hPutStrLn xmproc
      }

    , terminal = myTerminal

    , workspaces = myWorkspaces

    , borderWidth = 2
    , normalBorderColor = colBackground
    , focusedBorderColor = colForeground

    , modMask = myModMask
    , keys = myKeys
    }

-- Utilities
(..=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ..=? x = fmap (x `isPrefixOf`) q

(=..?) :: Eq a => Query [a] -> [a] -> Query Bool
q =..? x = fmap (x `isSuffixOf`) q

rectFloat r = \w -> W.float w r
maximizeRect = W.RationalRect 0 0 1 1
centerRect = W.RationalRect 0.13 0.1 0.74 0.8
largeRect = W.RationalRect 0.13 0.02 0.74 0.98
largeThinRect = W.RationalRect 0.3 0.02 0.4 0.98
