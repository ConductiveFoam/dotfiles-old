#!/usr/bin/sh
# Keyboard setup: US keymap, custom binds, enable numlock
setkeyboard.sh us
numlockx
export GTK_IM_MODULE=xim

# MX Ergo config
imwheel
solaar -w hide 2> /dev/null &

# Disable beeps
xset b off

systemctl --user start udiskied
