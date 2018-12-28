mon_primary='HDMI-1'
mon_secondary='DVI-D-1'
xrandr --output "$mon_primary" --primary --output "$mon_secondary" --left-of "$mon_primary" --rotate left
