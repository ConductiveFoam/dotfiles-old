#!/usr/bin/sh
setxkbmap -layout dvorak -option ctrl:nocaps -option compose:ralt
# Map XF86Back and XF86Forward from (browser) navigation
xmodmap -e "keycode 166 = Prior" -e "keycode 167 = Next"
# Unmap XF86Mail and XF86Favorites
xmodmap -e "keycode 163 = NoSymbol" -e "keycode 164 = NoSymbol"
xset b off
numlockx

systemctl --user start udiskied

xscreensaver -no-splash &
systemctl --user start xss-deactivate.timer
