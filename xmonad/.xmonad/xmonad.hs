-- Imports: Base
import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative (pure)
import Data.Default (def)
import Data.Foldable (foldl')
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Monoid (Endo(..), appEndo)
import qualified DBus.Notify as Notify
import System.IO (BufferMode(LineBuffering), FilePath, Handle, hPutStrLn, hSetBuffering)
import System.Posix.IO ( FdOption(CloseOnExec), setFdOption
                       , createPipe, fdToHandle, closeFd
                       , dupTo, stdInput
                       )
import System.Posix.Process (executeFile)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import XMonad
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)

-- X11
import Graphics.X11.Xlib.Types (Rectangle(..))
import Graphics.X11.Xinerama (getScreenInfo)

-- Imports: Hooks
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                               , ppCurrent, ppExtras, ppOrder, ppOutput
                               , ppSep, ppTitle, ppUrgent, ppVisible
                               , xmobarColor
                               , shorten, wrap
                               )
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers ( (-?>), composeOne
                                  , isDialog, isFullscreen, transience
                                  , doRectFloat, doHideIgnore, doCenterFloat, doFullFloat
                                  , doSideFloat, Side(SC)
                                  )

-- Imports: Layouts
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.Column (Column(Column))
import XMonad.Layout.Mosaic (mosaic)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)

-- Imports: Hotkey config
import XMonad.Actions.Submap (submap)
import XMonad.Actions.LoggedAction (withLog, logAction)
import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioMute, xF86XK_AudioPlay
                                    , xF86XK_Launch5, xF86XK_Launch6, xF86XK_Launch7, xF86XK_Launch8, xF86XK_Launch9
                                    )

-- Imports: Auxiliaries
import qualified MyBars
import qualified XMobar.Config as XMobar
import qualified XMonad.Prompt as XPrompt
import XMonad.Prompt.AssociationPrompt (associationPrompt)
import XMonad.Prompt.ListCompletedPrompt (listCompletedPrompt)
import XMonad.Prompt.MPD (loadPlaylist)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (WindowPrompt(Goto, Bring), allWindows, windowPrompt)

-- Imports: Actions
import qualified XMonad.Actions.MPD as MPD
import XMonad.Actions.WindowGo (raiseMaybe)
import XMonad.Actions.WithAll (withAll)
import qualified XMonad.StackSet as W
import XMonad.Util.Paste (pasteSelection)
import XMonad.Util.Run ( runInTerm, runProcessWithInput
                       , unsafeSpawn, safeSpawn, safeSpawnProg
                       )
import XMonad.Util.SpawnOnce (spawnOnce)

-- Imports: Custom
import CommandPrefix ( resetPrefix, decrementPrefix, prependToPrefix
                     , logPrefix, prefixToString
                     , prefixedAction, withPrefix
                     )
import MyVisuals

-- Workspaces
wsMain = "main"
wsDev = "dev"
wsRead = "read"
wsMedia = "media"
wsMsg = "msg"
wsMisc = "misc"
wsPile = "pile"
myWorkspaces = [ wsMain, wsDev, wsRead, wsMedia, wsMsg, wsMisc, wsPile ]

-- Terminal config
myTerminal = "alacritty"
termTitle = "Terminal"
termTitleTmux = "Terminal - Dev"
termTitleWeb = "Terminal - Web"
termTitleNotes = "Notes"
knownTerminalWindowClasses = ["Alacritty", "Xfce4-terminal"]
isTerminal :: Query Bool
isTerminal = anyOf (className =?) knownTerminalWindowClasses

-- Miscellaneous config
myModMask = mod4Mask
xvkbdWindowQuery = className =? "XVkbd"
xzoomWindowQuery = stringProperty "WM_ICON_NAME" =? "xzoom"

-- Layout hook
myLayout = onWorkspace wsDev col $ onWorkspace wsRead read $
           onWorkspace wsMedia noBorders' $ onWorkspace wsMsg msg $
           onWorkspace wsMisc noBorders' $ onWorkspace wsPile Accordion $
           tiled ||| (Mirror col) ||| (Mirror tiled)
  where
    col = Column 1
    read = Mirror $ (Tall 2 delta (1/3)) ||| (mosaic 2 [2, 1])
    msg = col ||| Mirror col
    tiled = Tall 1 delta (1/2)
    noBorders' = noBorders Full

    delta = 3/100

-- Manage hook
myManageHook = composeOne $
  -- Terminal and notification windows
  [ (isTerminal <&&> title =? t) -?> action
  | (t, action) <- terminalWindows
  ] ++
  [ (className =? "Zenity" <&&> title =? "xmonad key binds") -?> doRectFloat (W.RationalRect 0.15 0.03 0.7 0.94)
  , className =? "Zenity" -?> doRectFloat centerRect

  -- Gaming related
  , (className =? "Steam" <&&> title =? "Steam" ) -?> doShift wsMedia
  , (className =? "Steam" <&&> title ..=? "Friends List") -?> doShift wsMsg
  , (className =? "Steam" <&&> title ..=? "Steam - News") -?> doHideIgnore
  ] ++
  [ className =? t -?> action
  | (t, action) <- gameWindows
  ] ++

  -- Multimedia
  [ className =? cls -?> doShift wsMedia
  | cls <- multimediaWindows
  ] ++

  -- Messenger & Communication
  [ className =? "TelegramDesktop" -?> doShift wsMsg
  , className =? "TeamSpeak 3" -?> doShift wsMsg
  ] ++

  -- Firefox windows
  [(className =? "Firefox" <&&> anyOf (title .=.?) ts) -?> doShift ws | (ts, ws) <- firefoxWindows] ++
  [(className =? "Firefox" <&&> (fmap not isDialog)) -?> doShift wsMain

  -- Miscellaneous
  , className =? "Thunderbird" -?> doShift wsMisc
  , xvkbdWindowQuery -?> doSideFloat SC
  , xzoomWindowQuery -?> doRectFloat (W.RationalRect 0.05 0.55 0.4 0.4)

  , (isDialog <||> anyOf (className =?) dialogWindows) -?> doCenterFloat
  , isFullscreen -?> doFullFloat
  , transience
  ]
  where
    terminalWindows = [(termTitleTmux, doShift wsDev), (termTitleWeb, doShift wsRead), (termTitleNotes, doRectFloat notesRect)]
    gameWindows =
      [ ("Steam", doShift wsMedia)
      , ("Dustforce.bin.x86_64", doFullFloat >> doShift wsMedia)
      , ("hollow_knight.x86_64", doShift wsMedia)
      ]
    multimediaWindows = ["Blender", "vlc", "Inkscape"]
    firefoxWindows = [(["GitHub", "GitLab", "ArchWiki"], wsRead)]
    dialogWindows = ["Matplotlib", "feh"]

-- Key bindings
-- %% Modifier is windows (mod4) key
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = myModMask}) = M.fromList $
  -- %% ! Launching programs
  [ ((0, xF86XK_Launch5), safeSpawnProg "firefox") -- none-Hotkey1 %! Browser
  , ((0, xF86XK_Launch6), spawnTerminal termTitleTmux "tmuxinator start dev") -- none-Hotkey2 %! Dev terminal
  , ((0, xF86XK_Launch7), -- none-Hotkey3 %! Note taking terminal
      raiseMaybe (spawnTerminal termTitleNotes "tmuxinator start notes") notesWindowQuery)
  , ((0, xF86XK_Launch8), spawnVisualKeyboard) -- none-Hotkey4 %! Visual keyboard
  , ((0, xF86XK_Launch9), spawnMagnifier) -- none-Hotkey5 %! Screen magnifier
  , ((myModMask, xK_Return), spawnTerminal termTitleTmux "tmuxinator start dev") -- %! Dev terminal
  , ((myShiftMask, xK_Return), safeSpawn (terminal conf) ["-t", termTitle]) -- %! Bare terminal
  , ((myControlMask, xK_Return), spawnTerminal termTitleWeb "tmuxinator start web") -- %! Elinks terminal
  , ((myModMask, xK_r), withLog (colored colDGreen "Launch") $ submap $ M.fromList -- %! Prompts submap:
    [ ((0, xK_Return), listCompletedPrompt "Launch: " promptApps safeSpawnProg xpc) -- %! Programs from curated list
    , ((0, xK_g), associationPrompt "Start Game: " promptGames unsafeSpawn xpc) -- %! Game
    , ((shiftMask, xK_Return), shellPrompt xpc) -- %! Shell
    ])

  -- %% ! Moving focus
  , ((myModMask, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
  , ((myShiftMask, xK_Tab), windows W.focusUp) -- %! Move focus to the previous window
  , ((myModMask, xK_j), windows W.focusDown) -- %! Move focus to the next window
  , ((myModMask, xK_k), windows W.focusUp) -- %! Move focus to the previous window
  , ((myModMask, xK_m), windows W.focusMaster) -- %! Move focus to the master window
  , ((myModMask, xK_g), windowPrompt xpc Goto allWindows) -- %! Prompt for window and move focus there
  , ((myModMask, xK_b), windowPrompt xpc Bring allWindows) -- %! Prompt for window and bring it

  -- %% ! Actions on current window
  , ((myModMask, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling
  , ((myShiftMask, xK_f), withFocused $ windows . (rectFloat maximizeRect)) -- %! Float & maximize
  , ((myControlMask, xK_f), withFocused $ float) -- %! Float
  , ((myModMask, xK_f), withFocused $ windows . (rectFloat centerRect)) -- %! Float & center
  , ((myControlMask, xK_t), withFocused $ remanage) -- %! Reapply manage hook
  , ((myShiftControlMask, xK_t), withAll $ remanage) -- %! Reapply manage hook to current workspace
  , ((myShiftMask, xK_c), kill) -- %! Close the focused window

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

  -- %% ! Modifying the layout
  [ ((myModMask, xK_space), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
  , ((myShiftMask, xK_space), sendMessage FirstLayout) -- %! Reset the layouts on the current workspace to default
  , ((myModMask, xK_comma), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((myModMask, xK_period), sendMessage (IncMasterN (-1))) -- %! Decrement the number of windows in the master area
  , ((myModMask, xK_h), sendMessage Shrink) -- %! Shrink the master area
  , ((myModMask, xK_l), sendMessage Expand) -- %! Expand the master area

  -- %% ! MPD & Audio control
  , ((myShiftMask, xK_m), loadPlaylist MPD.withMPD xpc) -- %! Load playlist
  , ((0, xF86XK_AudioRaiseVolume), safeSpawn "amixer" ["set", "Master", "2%+"]) -- %! Increase volume
  , ((0, xF86XK_AudioLowerVolume), safeSpawn "amixer" ["set", "Master", "2%-"]) -- %! Decrease volume
  , ((0, xF86XK_AudioMute), safeSpawn "amixer" ["set", "Master", "toggle"]) -- %! Toggle mute
  , ((0, xF86XK_AudioPlay), MPD.toggle) -- %! MPD toggle

  , ((myControlMask, xK_m), withLog (colored colDGreen "MPD") $ submap $ M.fromList -- %! Audio Submap:
    [ ((0, xK_p), decrementPrefix 1 >> withPrefix MPD.play) -- %! MPD play $prefix
    , ((0, xK_n), MPD.pause True) -- %! MPD pause
    , ((shiftMask, xK_t), MPD.stop) -- %! MPD stop
    , ((0, xK_t), MPD.toggle) -- %! MPD toggle
    , ((0, xK_f), prefixedAction MPD.next) -- %! MPD next
    , ((0, xK_b), prefixedAction MPD.previous) -- %! MPD previous

    , ((0, xK_l), notifyOf "MPD - Playlist" MPD.showPlaylist) -- %! Notify of current playlist
    , ((0, xK_s), withPrefix notifySong) -- %! Notify of song $prefix

    , ((shiftMask, xK_s), MPD.toggleStatus MPD.Random) -- %! MPD random
    , ((shiftMask, xK_r), MPD.toggleStatus MPD.Repeat) -- %! MPD repeat
    , ((shiftMask, xK_c), MPD.toggleStatus MPD.Consume) -- %! MPD consume
    , ((shiftMask, xK_o), MPD.toggleStatus MPD.Single) -- %! MPD single

    , ((0, xK_Delete), withPrefix deleteSong) -- %! MPD del $prefix
    , ((controlMask, xK_Delete), MPD.clear) -- %! MPD clear
    ])

  -- %% ! Systemctl integration
  , ((myControlMask, xK_s), withLog (colored colDGreen "Unit") $ submap $ M.fromList -- %! Systemctl submap:
    [ ((0, xK_s), sysctlPrompt "Unit Status: " "status" xpc) -- %! Show unit status
    , ((0, xK_a), sysctlPrompt "Start Unit: " "start" xpc) -- %! Start unit
    , ((0, xK_d), sysctlPrompt "Stop Unit: " "stop" xpc) -- %! Stop unit
    , ((0, xK_r), sysctlPrompt "Restart Unit: " "restart" xpc) -- %! Restart unit
    ])

  -- %% ! Quit xmonad, Power control
  , ((myShiftMask, xK_q), liftIO (exitWith ExitSuccess)) -- %! Quit xmonad
  , ((myModMask, xK_q), safeSpawnProg "restart_xmonad.sh") -- %! Restart xmonad
  , ((myShiftControlMask, xK_q), safeSpawn "systemctl" ["poweroff"]) -- %! Shut off system
  , ((myModMask, xK_z), safeSpawn "xscreensaver-command" ["--lock"]) -- %! Lock screen
  , ((myControlMask, xK_z), safeSpawnProg "togglexss.sh") -- %! Toggle automatic lock

  -- %% ! Notification control
  -- mod-ctrl-n %! Close notification
  -- mod-ctrl-shift-n %! Close all notifications
  -- mod-shift-h %! Show previous notification(s)

  -- %% ! Miscellaneous
  , ((myControlMask, xK_space), safeSpawnProg "cyclexlayout.sh") -- %! Cycle keyboard layouts

  , ((myControlMask, xK_h), unsafeSpawn helpCommand) -- %! Key bind info window

  , ((0, xK_Insert), pasteSelection) -- %! Paste selection

  , ((controlMask, xK_Print), unsafeSpawn $ "sleep 0.2; " ++ (screenshotCommand "-s -c 0.42,0.44,0.77 -b 2" "-selection")) -- %! Select window or rectangle, make screenshot of it
  , ((shiftMask, xK_Print), withFocused $ \w -> unsafeSpawn $ screenshotCommand ("--window " ++ (show w)) "-window") -- %! Make screenshot of focused window
  , ((0, xK_Print), unsafeSpawn $ screenshotCommand "" "") -- %! Make screenshot of whole desktop

  , ((myControlMask, xK_g), resetPrefix >> refresh) -- %! Reset prefix

  ] ++
  -- mod-control-[0..9] %! Extend prefix
  [((myControlMask, key), (prependToPrefix $ (fromIntegral key) - 48) >> refresh) | key <- [xK_0 .. xK_9]]
  where
    myShiftMask = myModMask .|. shiftMask
    myControlMask = myModMask .|. controlMask
    myShiftControlMask = myModMask .|. shiftMask .|. controlMask

    notesWindowQuery = isTerminal <&&> title =? termTitleNotes

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

    screenshotCommand opts name = "maim " ++ opts ++ " $HOME/screenshots/screenshot" ++ name ++"-$(date +%Y%m%d-%H-%M-%S).png"
    helpCommand = "zenity --info --no-wrap --title=\"xmonad key binds\"" ++
      " --text=\"$(awk -f ~/.xmonad/genhelp.awk ~/.xmonad/xmonad.hs | sed 's/&/&amp;/')\""
    steamCommand = ("steam steam://run/" ++)

    notifySong 0 = MPD.currentSong >>= (maybe (notify "MPD" "No song") (notifySong . (+ 1)))
    notifySong p = notifyOf ("MPD - Song #" ++ (show p)) $ MPD.showSong (p - 1)
    deleteSong 0 = MPD.currentSong >>= (maybe (return ()) MPD.delete)
    deleteSong p = MPD.delete $ p - 1

    spawnTerminal title cmd = runInTerm ("-t \"" ++ title ++ "\"") cmd
    sysctlAction cmd unit = runProcessWithInput "systemctl" ["--user", cmd, unit] ""
    sysctlPrompt name "status" xpc = listCompletedPrompt name promptSysUnits ((notifyOf name) . sysctlAction "status") xpc
    sysctlPrompt name cmd xpc = listCompletedPrompt name promptSysUnits (\u -> sysctlAction cmd u >> notify name u) xpc

    spawnVisualKeyboard = raiseMaybe (spawn "xvkbd") xvkbdWindowQuery
    showClamped i | i < 0 = "0"
                  | otherwise = show i
    showPosition (x, y) = "-source 500x500+" ++ (showClamped x) ++ "+" ++ (showClamped y)
    spawnMagnifier = do
      conf <- ask
      let newPosition = (\(x, y) -> (x - 192, y - 108)) <$> mousePosition conf
      raiseMaybe (spawn $ "xzoom " ++ (maybe "" showPosition newPosition)) xzoomWindowQuery

    promptSysUnits = ["redshiftd.service", "xss-deactivate.timer", "dunst.service", "mpd.service"]
    promptApps =
      [ "firefox"
      , "alacritty"
      , "blender", "gimp", "inkscape", "libreoffice", "steam", "vlc"
      , "telegram-desktop", "teamspeak3"
      , "pavucontrol-qt"
      ]
    promptGames = M.fromList $
      [ ("Left 4 Dead 2", "left4gore -2")
      , ("Stellaris", steamCommand "281990")
      , ("Full Bore", steamCommand "264060")
      , ("Zen Bound 2",steamCommand "61600")
      , ("Portal", steamCommand "400")
      , ("Portal 2", steamCommand "620")
      , ("Hollow Knight", "$HOME/games/Hollow\\ Knight/start.sh")
      , ("They Bleed Pixels", steamCommand "211260")
      , ("Rex Rocket", steamCommand "288020")
      , ("Dustforce", steamCommand "65300")]

-- Main config
main = do
  -- Monitor-dependent infobar config
  dpy <- openDisplay ""
  rects <- getScreenInfo dpy
  let (primaryBarConfig, secondaryBarsConfig) =
        if (length rects) == 2 && (screenOrientation $ rects !! 1) == Vertical then
          (MyBars.mainScreenBar, [MyBars.loadBar, MyBars.musicBar])
        else
          (MyBars.primaryBar, [MyBars.secondaryBar])

  -- Main setup
  hInfoBar <- safeSpawnPipe "xmobar" $ XMobar.asList primaryBarConfig
  xmonad . ewmh . docks $ desktopConfig
    { manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = avoidStruts $ desktopLayoutModifiers myLayout
    , logHook = dynamicLogWithPP def
      { ppCurrent = colored colDYellow . wrap "(" ")"
      , ppVisible = colored colDBlue . wrap "(" ")"
      , ppUrgent = colored colDRed
      , ppTitle = colored colDMagenta . shorten titleLength

      , ppSep = " | "
      , ppExtras = [logAction, logPrefix $ (colored colDYellow) . prefixToString]
      , ppOrder = \(ws:_:t:ex) -> ex ++ [ws,t]
      , ppOutput = hPutStrLn hInfoBar
      }
    , startupHook = mapM_ id $ [tray, nitrogen] ++ (xmobar <$> secondaryBarsConfig)

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

    tray = spawnOnce $ "stalonetray -bg \"" ++  colBackground ++ "\""
    nitrogen = safeSpawn "nitrogen" ["--restore"]
    xmobar cfg = safeSpawn "xmobar" $ XMobar.asList cfg

-- Utilities
(..=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ..=? x = fmap (x `isPrefixOf`) q

(.=.?) :: Eq a => Query [a] -> [a] -> Query Bool
q .=.? x = fmap (x `isInfixOf`) q

(=..?) :: Eq a => Query [a] -> [a] -> Query Bool
q =..? x = fmap (x `isSuffixOf`) q

anyOf :: (a -> Query Bool) -> [a] -> Query Bool
anyOf q rs = foldl' (<||>) (pure False) $ q <$> rs

rectFloat :: Ord a => W.RationalRect -> a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
rectFloat = flip W.float

maximizeRect = W.RationalRect 0 0 1 1
centerRect = W.RationalRect 0.13 0.1 0.74 0.8
largeRect = W.RationalRect 0.13 0.02 0.74 0.98
largeThinRect = W.RationalRect 0.3 0.02 0.4 0.98
notesRect = W.RationalRect 0.60 0.02 0.37 0.7

remanage :: Window -> X ()
remanage w = do
  mh <- asks (manageHook . config)
  g <- appEndo <$> userCodeDef (Endo id) (runQuery mh w)
  windows g

safeSpawnPipe :: MonadIO m => FilePath -> [String] -> m Handle
safeSpawnPipe prog args = liftIO $ do
  (rd, wr) <- createPipe
  setFdOption wr CloseOnExec True
  h <- fdToHandle wr
  hSetBuffering h LineBuffering
  _ <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile (encodeString prog) True (map encodeString args) Nothing
  closeFd rd
  return h

notify :: MonadIO m => String -> String -> m ()
notify s b = notifyOf s (return b)

notifyOf :: MonadIO m => String -> m String -> m ()
notifyOf s a = do
  b <- a
  client <- liftIO Notify.connectSession
  let note = Notify.blankNote { Notify.summary = s
                              , Notify.body = (Just $ Notify.Text b)
                              }
  notification <- liftIO $ Notify.notify client note
  return ()

colored :: String -> String -> String
colored col = xmobarColor col ""

data ScreenOrientation = Horizontal | Vertical
  deriving (Eq, Show, Read)

screenOrientation :: Rectangle -> ScreenOrientation
screenOrientation Rectangle { rect_width = w, rect_height = h }
  | h < w = Horizontal
  | otherwise = Vertical
