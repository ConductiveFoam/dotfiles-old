module XMobar.Config.Types
    ( Config (..)
    , XPosition (..), Align (..), Border(..), Runnable(..)
    , asList, defaultXMobarConfig
    ) where

import Data.Default
import Data.List (intercalate)

-- import Xmobar.Run.Runnable (Runnable(..))

data Config =
  Config { font :: String         -- ^ Font
         , additionalFonts :: [String] -- ^ List of alternative fonts
         , wmClass :: String      -- ^ X11 WM_CLASS property value
         , wmName :: String       -- ^ X11 WM_NAME property value
         , bgColor :: String      -- ^ Backgroud color
         , fgColor :: String      -- ^ Default font color
         , position :: XPosition  -- ^ Top Bottom or Static
         , textOffset :: Int      -- ^ Offset from top of window for text
         , textOffsets :: [Int]   -- ^ List of offsets for additionalFonts
         , iconOffset :: Int      -- ^ Offset from top of window for icons
         , border :: Border       -- ^ NoBorder TopB BottomB or FullB
         , borderColor :: String  -- ^ Border color
         , borderWidth :: Int     -- ^ Border width
         , alpha :: Int           -- ^ Transparency from 0 (transparent)
                    --   to 255 (opaque)
         , hideOnStart :: Bool    -- ^ Hide (Unmap) the window on
                          --   initialization
         , allDesktops :: Bool    -- ^ Tell the WM to map to all desktops
         , overrideRedirect :: Bool -- ^ Needed for dock behaviour in some
                               --   non-tiling WMs
         , pickBroadest :: Bool   -- ^ Use the broadest display
                           --   instead of the first one by
                           --   default
         , lowerOnStart :: Bool   -- ^ lower to the bottom of the
                           --   window stack on initialization
         , persistent :: Bool     -- ^ Whether automatic hiding should
                         --   be enabled or disabled
         , iconRoot :: FilePath   -- ^ Root folder for icons
         , commands :: [Runnable] -- ^ For setting the command,
                       --   the command arguments
                       --   and refresh rate for the programs
                       --   to run (optional)
         , sepChar :: String      -- ^ The character to be used for indicating
                      --   commands in the output template
                      --   (default '%')
         , alignSep :: String     -- ^ Separators for left, center and
                       --   right text alignment
         , template :: String     -- ^ The output template
         , verbose :: Bool        -- ^ Emit additional debug messages
         }

instance Default Config where
  def = defaultXMobarConfig

defaultXMobarConfig :: Config
defaultXMobarConfig =
  Config { font = ""
         , additionalFonts = []
         , wmClass = "xmobar"
         , wmName = "xmobar"
         , bgColor = ""
         , fgColor = ""
         , position = Top
         , textOffset = 0
         , textOffsets = []
         , iconOffset = 0
         , border = NoBorder
         , borderColor = ""
         , borderWidth = 0
         , alpha = 255
         , hideOnStart = False
         , allDesktops = True
         , overrideRedirect = True
         , pickBroadest = False
         , lowerOnStart = False
         , persistent = False
         , iconRoot = "."
         , commands = []
         , sepChar = "%"
         , alignSep = "}{"
         , template = "%StdinReader% }{ "
         , verbose = False
         }

asList :: Config -> [String]
asList Config { font = font
               , bgColor = bgColor
               , fgColor = fgColor
               , position = position
               , commands = commands
               , template = template
               } =
  [ "-B", bgColor, "-F", fgColor
  , "-f", font
  , "-p", (show position)
  , "-c", (show commands)
  , "-t", template
  ]

data Runnable =
  Run { name :: String
           , literal :: [String]
           , quoted :: [String]
           }

instance Show Runnable where
  show (Run name literals quoted) = "Run " ++ name ++ " " ++ quoted' ++ " " ++ literals'
    where
      quote x = "\"" ++ x ++ "\""
      quoted' | quoted == [] = ""
              | otherwise = "[" ++ (intercalate ", " (map quote quoted)) ++ "]"
      literals' = intercalate " " literals

data XPosition = Top
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
                 deriving ( Show, Read, Eq )

data Align = L | R | C deriving ( Show, Read, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Show, Read, Eq )
