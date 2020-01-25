#!/usr/bin/sh
# Keyboard setup: Dvorak keymap, custom binds, enable numlock
setkeyboard.sh dvorak
numlockx

# MX Ergo config
imwheel
solaar 2> /dev/null &

# Disable beeps
xset b off

# Screensaver: Startup, deactivation timer
xscreensaver -no-splash &
systemctl --user start xss-deactivate.timer

systemctl --user start udiskied
