#!/usr/bin/sh
setxkbmap -layout dvorak -option ctrl:nocaps -option compose:ralt
xset b off
numlockx

systemctl --user start udiskied

xscreensaver -no-splash &
systemctl --user start xss-deactivate.timer
