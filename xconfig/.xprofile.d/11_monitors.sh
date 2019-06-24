if [[ $(xrandr | grep " connected" | wc -l) -eq 2 ]]; then
    mon_primary='HDMI-0'
    mon_secondary='DVI-D-0'
    xrandr --output "$mon_primary" --primary --output "$mon_secondary" --left-of "$mon_primary" --rotate left
else
    xrandr --output "$(xrandr | grep " connected" | cut -d' ' -f1)" --primary
fi
