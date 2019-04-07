-- Imports: Base
import Control.Monad (liftM)
import Data.Default (def)
import qualified Data.Map as M
import Data.Monoid (appEndo, Endo(..))
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import System.IO (Handle, FilePath, BufferMode(LineBuffering), hPutStrLn, hSetBuffering)
import System.Posix.IO (FdOption(CloseOnExec), setFdOption
                       , createPipe, fdToHandle, closeFd
                       , dupTo, stdInput
                       )
import System.Posix.Process (executeFile)
import System.Exit (exitWith, ExitCode(ExitSuccess))

-- Imports: Other
import qualified Network.MPD as MPD
import Codec.Binary.UTF8.String (encodeString)

-- Imports: XMonad base
import XMonad
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP
                               , ppCurrent, ppExtras, ppOrder, ppOutput
                               , ppSep, ppTitle, ppUrgent, ppVisible
                               , xmobarColor
                               , shorten, wrap
                               )
import XMonad.Hooks.ManageHelpers (composeOne, (-?>)
                                  , isDialog, isFullscreen, transience
                                  , doRectFloat, doHideIgnore, doCenterFloat, doFullFloat
                                  )

-- Imports: Layouts
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.Column (Column(Column))
import XMonad.Layout.Mosaic (mosaic)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)

-- Imports: XMobar
import qualified XMobar.Config as XMobar

-- Imports: Prompt
import qualified XMonad.Prompt as XPrompt
import XMonad.Prompt.MPD (loadPlaylist)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.AssociationPrompt (associationPrompt)
import XMonad.Prompt.ListCompletedPrompt (listCompletedPrompt)

-- Imports: Hotkey config
import XMonad.Actions.Submap (submap)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioMute, xF86XK_AudioPlay
                                    )

-- Imports: Actions
import XMonad.Actions.WithAll (withAll)
import qualified XMonad.StackSet as W
import XMonad.Util.Run (runInTerm, unsafeSpawn, safeSpawn)
import XMonad.Util.Paste (pasteSelection)

-- Imports: Custom
import MyColors (colBackground, colForeground
                , colDBlue, colDGreen
                , colDMagenta, colDRed
                , colDYellow
                )
import CommandPrefix (modifyPrefix, resetPrefix, withPrefix, logPrefix)

-- Workspaces
wsMain = "main"
wsDev = "dev"
wsRead = "read"
wsGame = "game"
wsMsg = "msg"
wsMisc = "misc"
myWorkspaces = [ wsMain, wsDev, wsRead, wsGame, wsMsg, wsMisc ]

-- Terminal window titles
termTitle = "Terminal"
termTitleTmux = "Terminal - Dev"
termTitleWeb = "Terminal - Web"

-- Miscellaneous config
myTerminal = "alacritty"
myFont = "xft:Fira Code:style=Bold:size=9:antialias=true"
myModMask = mod4Mask

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
  -- Terminal and xmessage
  [ (className =? terminal <&&> title =? t) -?> action
  | terminal <- knownTerminalClasses
  , (t, action) <- managedTerminalWindows
  ] ++
  [ (className =? "Xmessage" <&&> title =? "xmonad key binds") -?> doRectFloat (W.RationalRect 0.3 0 0.4 1)
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
  ] ++

  -- Firefox windows
  [(className =? "Firefox" <&&> title .=.? t) -?> doShift ws | (ts, ws) <- managedFirefoxWindows, t <- ts] ++
  [(className =? "Firefox" <&&> (notQuery isDialog)) -?> doShift wsMain

  -- Miscellaneous
  , className =? "vlc" -?> doShift wsMisc
  , className =? "Thunderbird" -?> doShift wsMisc
  , className =? "Matplotlib" -?> doCenterFloat
  , className =? "feh" -?> doCenterFloat

  , isDialog -?> doCenterFloat
  , isFullscreen -?> doFullFloat
  , transience
  ]
  where
    knownTerminalClasses = ["Alacritty", "Xfce4-terminal"]
    managedTerminalWindows = [(termTitleTmux, doShift wsDev), (termTitleWeb, doShift wsRead)]

    managedFirefoxWindows = [(["GitHub", "GitLab", "ArchWiki"], wsRead)]

-- Key bindings
-- %% Modifier is windows (mod4) key
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = myModMask}) = M.fromList $
  -- %% ! Launching and Killing programs
  [ ((myShiftMask, xK_Return), safeSpawn (terminal conf) ["-t", termTitle]) -- %! Launch terminal
  , ((myModMask, xK_Return), spawnTerminal termTitleTmux "tmux a -t dev") -- %! Launch dev terminal
  , ((myControlMask, xK_Return), spawnTerminal termTitleWeb "tmux a -t web") -- %! Launch elinks terminal
  , ((myShiftMask, xK_r), shellPrompt xpc) -- %! Launch app
  , ((myModMask, xK_r), listCompletedPrompt "Launch: " promptApps unsafeSpawn xpc) -- %! Launch app from curated list
  , ((myModMask, xK_g), associationPrompt "Start Game: " promptGames unsafeSpawn xpc) -- %! Launch game
  , ((myShiftMask, xK_c), kill) -- %! Close the focused window

  , ((myModMask, xK_space), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
  , ((myShiftMask, xK_space), sendMessage FirstLayout) -- %!  Reset the layouts on the current workspace to default

  , ((myModMask, xK_n), refresh) -- %! Refresh xmonad

  -- %% ! Moving focus
  , ((myModMask, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
  , ((myShiftMask, xK_Tab), windows W.focusUp) -- %! Move focus to the previous window
  , ((myModMask, xK_j), windows W.focusDown) -- %! Move focus to the next window
  , ((myModMask, xK_k), windows W.focusUp) -- %! Move focus to the previous window
  , ((myModMask, xK_m), windows W.focusMaster) -- %! Move focus to the master window

  -- %% ! Actions on current window
  , ((myModMask, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling
  , ((myShiftMask, xK_f), withFocused $ windows . (rectFloat maximizeRect)) -- %! Float & maximize
  , ((myControlMask, xK_f), withFocused $ float) -- %! Float
  , ((myModMask, xK_f), withFocused $ windows . (rectFloat centerRect)) -- %! Float & center
  , ((myControlMask, xK_t), withFocused $ remanage) -- %! Reapply manage hook
  , ((myShiftControlMask, xK_t), withAll $ remanage) -- %! Reapply manage hook to current workspace

  -- %% ! Modifying the window order
  , ((myShiftControlMask, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
  , ((myShiftMask, xK_j), windows W.swapDown) -- %! Swap the focused window with the next window
  , ((myShiftMask, xK_k), windows W.swapUp) -- %! Swap the focused window with the previous window
  ] ++

  -- %% ! Workspaces & Screens
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip (myWorkspaces) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  -- mod-{w,e} %! Switch to physical/Xinerama screens 1, 2
  -- mod-shift-{w,e} %! Move client to screen 1, 2
  [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e] [0..] --, xK_r
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ] ++

  -- %% ! Increasing or Decreasing number of windows in the master area
  [ ((myModMask, xK_comma), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((myModMask, xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

  -- %% ! Resizing the master/slave ratio
  , ((myModMask, xK_h), sendMessage Shrink) -- %! Shrink the master area
  , ((myModMask, xK_l), sendMessage Expand) -- %! Expand the master area

  -- %% ! MPD & Audio control
  , ((myShiftMask, xK_m), loadPlaylist MPD.withMPD xpc) -- %! Load playlist
  , ((myControlMask, xK_m), submap . M.fromList $ -- %! Audio Submap:
    [ ((0, xK_p), withPrefix $ (\p -> liftMPD_ $ MPD.play (Just p))) -- %! MPC play $prefix
    , ((0, xK_n), liftMPD_ $ MPD.pause True) -- %! MPC pause
    , ((shiftMask, xK_t), liftMPD_ $ MPD.stop) -- %! MPC stop
    , ((0, xK_t), toggle) -- %! MPC toggle
    , ((0, xK_f), liftMPD_ $ MPD.next) -- %! MPC next
    , ((0, xK_b), liftMPD_ $ MPD.previous) -- %! MPC previous

    , ((0, xK_l), unsafeSpawn $ "notify-send \"$(mpc playlist | awk '{ print (NR - 1) \": \" $0 }')\"") -- %! Notify of current playlist

    , ((0, xK_s), invert MPD.stRandom MPD.random) -- %! MPC random
    , ((0, xK_r), invert MPD.stRepeat MPD.repeat) -- %! MPC repeat
    , ((0, xK_c), invert MPD.stConsume MPD.consume) -- %! MPC consume
    , ((0, xK_o), invert MPD.stSingle MPD.single) -- %! MPC single

    , ((0, xK_Delete), withPrefix (\p -> liftMPD_ $ MPD.delete p)) -- %! MPC del $prefix
    , ((controlMask, xK_Delete), liftMPD_ $ MPD.clear) -- %! MPC clear
    ])
  , ((0, xF86XK_AudioRaiseVolume), unsafeSpawn "amixer set Master 2%+") -- %! Increase volume
  , ((0, xF86XK_AudioLowerVolume), unsafeSpawn "amixer set Master 2%-") -- %! Decrease volume
  , ((0, xF86XK_AudioMute), unsafeSpawn "amixer set Master toggle") -- %! Toggle mute
  , ((0, xF86XK_AudioPlay), toggle) -- %! MPC toggle

  -- %% ! Systemctl integration
  , ((myControlMask, xK_s), submap . M.fromList $ -- %! Systemctl submap:
    [ ((0, xK_s), sysctlPrompt "Unit Status: " "status" xpc) -- %! Show unit status
    , ((0, xK_t), sysctlPrompt "Toggle Unit: " "toggle" xpc) -- %! Toggle unit
    , ((0, xK_a), sysctlPrompt "Start Unit: " "start" xpc) -- %! Start unit
    , ((0, xK_d), sysctlPrompt "Stop Unit: " "stop" xpc) -- %! Stop unit
    , ((0, xK_r), sysctlPrompt "Restart Unit: " "restart" xpc) -- %! Restart unit
    ])

  -- %% ! Quit xmonad, Power control
  , ((myShiftMask, xK_q), io (exitWith ExitSuccess)) -- %! Quit xmonad
  , ((myModMask, xK_q), unsafeSpawn "if type xmonad; then xmonad --recompile && (pkill xmobar; xmonad --restart); else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
  , ((myShiftControlMask, xK_q), unsafeSpawn "systemctl poweroff") -- %! Shut off system
  , ((myModMask, xK_z), unsafeSpawn "xscreensaver-command --lock") -- %! Lock screen
  , ((myControlMask, xK_z), unsafeSpawn "togglexss.sh") -- %! Toggle automatic lock

  -- %% ! Notification control
  -- mod-ctrl-n %! Close notification
  -- mod-ctrl-shift-n %! Close all notifications
  -- mod-shift-h %! Show previous notification(s)

  -- %% ! Miscellaneous
  , ((myControlMask, xK_space), unsafeSpawn "cyclexlayout.sh") -- %! Cycle keyboard layouts

  , ((myControlMask, xK_h), helpCommand) -- %! XMessage window with key binds

  , ((0, xK_Insert), pasteSelection) -- %! Paste selection

  , ((controlMask, xK_Print), unsafeSpawn $ "sleep 0.2; " ++ (makeScreenshotCommand "-s -c 0.42,0.44,0.77 -b 2" "-selection")) -- %! Select window or rectangle, make screenshot of it
  , ((shiftMask, xK_Print), withFocused $ \w -> unsafeSpawn $ makeScreenshotCommand ("--window " ++ (show w)) "-window") -- %! Make screenshot of focused window
  , ((0, xK_Print), unsafeSpawn $ makeScreenshotCommand "" "") -- %! Make screenshot of whole desktop

  , ((myControlMask, xK_g), resetPrefix >> refresh)
  ] ++ -- %! Reset prefix
  -- mod-control-[0..9] %! Extend prefix
  [((myControlMask, key), (modifyPrefix $ (fromIntegral key) - 48) >> refresh) | key <- [xK_0 .. xK_9]]
  where
    myShiftMask = myModMask .|. shiftMask
    myControlMask = myModMask .|. controlMask
    myShiftControlMask = myModMask .|. shiftMask .|. controlMask

    helpCommand :: X ()
    helpCommand = unsafeSpawn $ "awk -f $HOME/.xmonad/genhelp.awk $HOME/.xmonad/xmonad.hs | " ++
      "xmessage -buttons '' -title \"xmonad key binds\" -file -"

    -- Prompt
    xpc = def
      { XPrompt.font = myFont
      , XPrompt.bgColor = colBackground
      , XPrompt.fgColor = colForeground
      , XPrompt.bgHLight = colForeground
      , XPrompt.fgHLight = colBackground
      , XPrompt.promptBorderWidth = 0
      , XPrompt.position = XPrompt.Top
      }

    spawnTerminal title cmd = runInTerm ("-t \"" ++ title ++ "\"") cmd
    makeScreenshotCommand opts name = "maim " ++ opts ++ " $HOME/screenshots/screenshot" ++ name ++"-$(date +%Y%m%d-%H-%M-%S).png"
    sysctlPrompt name cmd xpc = listCompletedPrompt name promptSysUnits (spawnSysctl cmd) xpc
    spawnSysctl cmd unit = spawn $ "sysdctl.sh " ++ cmd ++ " " ++ unit
    spawnSteam = ("steam steam://run/" ++)

    promptSysUnits = ["redshiftd.service", "xss-deactivate.timer", "dunst.service", "mpd.service"]
    promptApps = ["firefox", "steam", "alacritty", "telegram-desktop", "teamspeak3", "vlc", "pavucontrol-qt", "libreoffice"]
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

    -- MPD
    liftMPD = liftIO . MPD.withMPD
    liftMPD_ = liftM (const ()) . liftMPD

    invert g s = liftMPD_ $ do
      status <- MPD.status
      s $ not (g status)
    toggle = liftMPD_ $ do
      MPD.Status { MPD.stState = s } <- MPD.status
      case s of
        MPD.Playing -> MPD.pause True
        _ -> MPD.play Nothing

-- Main config
main = do
  safeSpawnOnce "stalonetray" ["-bg", wrap "'" "'" colBackground]

  mapM_ (\ (cmd, ps) -> safeSpawn cmd ps) startupApplications

  xmproc <- safeSpawnPipe "xmobar" (XMobar.asList primaryBar)
  xmonad $ ewmh $ desktopConfig
    { manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = desktopLayoutModifiers $ myLayout
    , logHook = dynamicLogWithPP def
      { ppCurrent = xmobarColor colDBlue "" . wrap "[" "]"
      , ppVisible = wrap "(" ")"
      , ppUrgent = xmobarColor colDRed colDYellow
      , ppTitle = xmobarColor colDMagenta "" . shorten titleLength

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
  where
    titleLength = 150

    -- Startup
    safeSpawnOnce cmd ps = spawn $
      "if [[ ! $(pgrep " ++ cmd ++ ") ]]; then " ++ cmd ++ " " ++ (intercalate " " ps) ++ ";fi"
    startupApplications = [ ("nitrogen", ["--restore"])
                          , xmobar loadBar
                          , xmobar musicBar
                          ]

    -- XMobar
    xmobar cfg = ("xmobar", XMobar.asList cfg)

    colConfig = ["-l", colDBlue, "-n", colDGreen, "-h", colDRed]
    myDefaultXMobar = def
      { XMobar.font = myFont
      , XMobar.bgColor = colBackground
      , XMobar.fgColor = colForeground
      }
    secondaryScreenBar = myDefaultXMobar
      { XMobar.screen = "1"
      , XMobar.alpha = 220
      }
    primaryBar = myDefaultXMobar
      { XMobar.position = XMobar.Static 1080 0 1844 19
      , XMobar.wmName = "XMobar - Main"
      , XMobar.commands = [ XMobar.Run "Kbd" ["[(\"us(dvorak)\", \"DV\"), (\"us\", \"US\"), (\"de\", \"DE\")]"] []
                          , XMobar.Run "Date" ["\"%l:%M %p\"", "\"time\"", "60"] []
                          , XMobar.Run "Uptime" ["[]", "60"] []

                          , XMobar.Run "StdinReader" [] []
                          ]
      , XMobar.template = " %StdinReader% }{ <fc=" ++ colDMagenta ++ ">%kbd%</fc> | <fc=" ++ colDMagenta ++ ">%xmobar_time.sh% %time%</fc> * %uptime% "
      }
    loadBar = secondaryScreenBar
      { XMobar.position = XMobar.Top
      , XMobar.wmName = "XMobar - Load"
      , XMobar.commands = [ XMobar.Run "Cpu" ["10"] $ colConfig ++ ["-L", "15", "-H", "50"]
                          , XMobar.Run "DynNetwork" ["10"] ["-t", "<dev>: <fc=" ++ colDBlue ++ "><rx></fc>;<fc=" ++ colDBlue ++ "><tx></fc>KB"]
                          , XMobar.Run "Memory" ["10"] $ colConfig ++ ["-L", "15", "-H", "50", "-t", "Mem: <usedratio>"]
                          , XMobar.Run "Swap" ["10"] $ colConfig ++ ["-L", "15", "-H", "50"]
                          ]
      , XMobar.template = " %dynnetwork% | XSS %xmobar_xssmode.sh% }{ %cpu% * Temp: %xmobar_coretemp.sh% | %memory%%xmobar_gpumem.sh% * %swap% "
      }
    musicBar = secondaryScreenBar
      { XMobar.position = XMobar.Bottom
      , XMobar.wmName = "XMobar - Music"
      , XMobar.template = " %xmobar_mpcstatus.sh% }{ %xmobar_volume.sh% "
      }

-- Utilities
(..=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ..=? x = fmap (x `isPrefixOf`) q

(.=.?) :: Eq a => Query [a] -> [a] -> Query Bool
q .=.? x = fmap (x `isInfixOf`) q

(=..?) :: Eq a => Query [a] -> [a] -> Query Bool
q =..? x = fmap (x `isSuffixOf`) q

notQuery :: Query Bool -> Query Bool
notQuery q = fmap not q

rectFloat :: Ord a => W.RationalRect -> a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
rectFloat r = \w -> W.float w r

maximizeRect = W.RationalRect 0 0 1 1
centerRect = W.RationalRect 0.13 0.1 0.74 0.8
largeRect = W.RationalRect 0.13 0.02 0.74 0.98
largeThinRect = W.RationalRect 0.3 0.02 0.4 0.98

remanage :: Window -> X ()
remanage w = do
  mh <- asks (manageHook . config)
  g <- appEndo <$> userCodeDef (Endo id) (runQuery mh w)
  windows g

safeSpawnPipe :: MonadIO m => FilePath -> [String] -> m Handle
safeSpawnPipe prog args = io $ do
  (rd, wr) <- createPipe
  setFdOption wr CloseOnExec True
  h <- fdToHandle wr
  hSetBuffering h LineBuffering
  _ <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile (encodeString prog) True (map encodeString args) Nothing
  closeFd rd
  return h
