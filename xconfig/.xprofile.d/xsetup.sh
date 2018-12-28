#!/usr/bin/sh
mon_primary='HDMI-1'
mon_secondary='DVI-D-1'
xrandr --output "$mon_primary" --primary --output "$mon_secondary" --left-of "$mon_primary" --rotate left

setxkbmap -layout dvorak -option ctrl:nocaps -option compose:ralt
xset b off
numlockx

systemctl --user start udiskied

xscreensaver -no-splash &
systemctl --user start xss-deactivate.timer
