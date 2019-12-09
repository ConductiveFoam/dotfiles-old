#! /usr/bin/bash
xmonad --recompile
if [[ $? == 0 ]]; then
    pkill xmobar; xmonad --restart
else
    zenity --error --no-wrap --no-markup --title "xmonad compilation errors" --text="$(cat ~/.xmonad/xmonad.errors)"
fi
