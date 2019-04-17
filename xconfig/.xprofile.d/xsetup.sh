#!/usr/bin/sh
setkeyboard.sh dvorak
xset b off
numlockx

systemctl --user start udiskied

xscreensaver -no-splash &
systemctl --user start xss-deactivate.timer
