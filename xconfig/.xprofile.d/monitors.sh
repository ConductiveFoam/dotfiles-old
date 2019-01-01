mon_primary='HDMI-0'
mon_secondary='DVI-D-0'
xrandr --output "$mon_primary" --primary --output "$mon_secondary" --left-of "$mon_primary" --rotate left
