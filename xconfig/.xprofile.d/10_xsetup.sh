#!/usr/bin/sh
# Keyboard setup: Dvorak keymap, custom binds, enable numlock
setkeyboard.sh dvorak
numlockx
export GTK_IM_MODULE=xim

# MX Ergo config
imwheel
solaar -w hide 2> /dev/null &

# Disable beeps
xset b off

# Screensaver: Startup, deactivation timer
xscreensaver -no-splash &
systemctl --user start xss-deactivate.timer

systemctl --user start udiskied
