module MyVisuals where

myFont
  , colDBlue, colLBlue
  , colDBlack, colLBlack
  , colDRed, colLRed
  , colDGreen, colLGreen
  , colDYellow, colLYellow
  , colDMagenta, colLMagenta
  , colDWhite, colLWhite
  , colBackground, colForeground :: String

myFont = "xft:Fira Code:style=Bold:size=9:antialias=true"

-- Solarized colors
colDBlue = "#268bd2"
colLBlue = "#839496"
colDBlack = "#073642"
colLBlack = "#002b36"
colDRed = "#dc322f"
colLRed = "#cb4b16"
colDGreen = "#859900"
colLGreen = "#586e75"
colDYellow = "#b58900"
colLYellow = "#657b83"
colDMagenta = "#d33682"
colLMagenta = "#6c71c4"
colDWhite = "#eee8d5"
colLWhite = "#fdf6e3"

-- dark / light
colBackground = colLBlack
colForeground = colLBlue
