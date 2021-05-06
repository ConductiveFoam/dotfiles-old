if [[ $(xrandr | grep " connected" | wc -l) -eq 2 ]]; then
    mon_primary='DisplayPort-0'
    mon_secondary='DisplayPort-1'
    xrandr --output "$mon_primary" --primary --output "$mon_secondary" --left-of "$mon_primary" --rotate left
else
    xrandr --output "$(xrandr | grep " connected" | cut -d' ' -f1)" --primary
fi

[ -f ~/.fehbg ] && ~/.fehbg
