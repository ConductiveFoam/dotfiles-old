module XMobar.Config
    ( Config (..)
    , XPosition (..), Align (..), Border(..), Runnable(..)
    , asList, defaultXMobarConfig
    ) where

-- Imports
import Data.Default
import Data.List (intercalate)

-- Config
data Config =
  Config { font :: String
         , iconRoot :: FilePath
         , wmClass :: String
         , wmName :: String

         , position :: XPosition
         , screen :: String
         , bgColor :: String
         , fgColor :: String
         , alpha :: Int

         , commands :: [Runnable]
         , sepChar :: String
         , alignSep :: String
         , template :: String

         , verbose :: Bool
         , dock :: Bool

         , file :: String
         }

instance Default Config where
  def = defaultXMobarConfig

defaultXMobarConfig :: Config
defaultXMobarConfig =
  Config { font = ""
         , iconRoot = "."
         , wmClass = "xmobar"
         , wmName = "xmobar"

         , position = Top
         , screen = "0"
         , bgColor = ""
         , fgColor = ""
         , alpha = 255

         , commands = []
         , sepChar = "%"
         , alignSep = "}{"
         , template = "%StdinReader% }{ "

         , verbose = False
         , dock = False

         , file = ""
         }

asList :: Config -> [String]
asList conf =
  [ "-f", font conf
  , "-i", iconRoot conf
  , "-w", wmClass conf
  , "-n", wmName conf

  , "-p", (show $ position conf)
  , "-x", screen conf
  , "-B", bgColor conf
  , "-F", fgColor conf
  , "-A", (show $ alpha conf)

  , "-c", (show $ commands conf)
  , "-s", sepChar conf
  , "-a", alignSep conf
  , "-t", template conf
  ] ++
  verbose' ++
  dock' ++
  file'
  where
    file' | (file conf) == "" = []
           | otherwise = [file conf]
    verbose' | verbose conf = ["-V"]
             | otherwise = []
    dock' | dock conf = ["-d"]
          | otherwise = []

-- Runnable
data Runnable =
  Run { name :: String
      , literal :: [String]
      , quoted :: [String]
      }

instance Show Runnable where
  show (Run rname literals rquoted) = "Run " ++ rname ++ " " ++ quoted' ++ " " ++ literals'
    where
      quote x = "\"" ++ x ++ "\""
      quoted' | rquoted == [] = ""
              | otherwise = "[" ++ (intercalate ", " (map quote rquoted)) ++ "]"
      literals' = intercalate " " literals

-- Auxiliaries
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
