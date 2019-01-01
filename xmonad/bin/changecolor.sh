#!/usr/bin/bash
# Replace fore- and background colors in MyColors.hs, .xmobarrc and dunstrc, invert base colors in .Xresources
# Done through three-step replace
color_replacements=(
    # .Xresources
    ': S_base0\([[:digit:]]\)$: S_base0\1;'
    ': S_base\([[:digit:]]\)$: S_base0\1'
    ': S_base0\([[:digit:]]\);$: S_base\1'

    # MyColors.hs
    'colBackground = colLBlack$colLBa'
    'colBackground = colLWhite$colBackground = colLBlack'
    'colLBa$colBackground = colLWhite'
    'colForeground = colLBlue$colLBu'
    'colForeground = colLYellow$colForeground = colLBlue'
    'colLBu$colForeground = colLYellow'

    # .xmobarrc
    'bgColor = "#002b36"$colLBa'
    'bgColor = "#fdf6e3"$bgColor = "#002b36"'
    'colLBa$bgColor = "#fdf6e3"'
    'fgColor = "#839496"$colBu'
    'fgColor = "#657b83"$fgColor = "#839496"'
    'colBu$fgColor = "#657b83"'

    # dunstrc
    'background = "#002b36"$colLBa'
    'background = "#fdf6e3"$background = "#002b36"'
    'colLBa$background = "#fdf6e3"'
    'frame_color = "#839496"$colLBu'
    'frame_color = "#657b83"$frame_color = "#839496"'
    'colLBu$frame_color = "#657b83"'
)
for row in "${color_replacements[@]}"; do
    original="$(echo $row | cut -d\$ -f1)"
    new="$(echo $row | cut -d\$ -f2)"
    sed --follow-symlinks -i -e "s/${original}\$/${new}/g" ~/.Xresources ~/.xmonad/lib/MyColors.hs ~/.xmobarrc ~/.config/dunst/dunstrc
done

# Kill trayer, restart xmonad, trayer and dunst, merge .Xresources
pkill trayer
xmonad --recompile && xmonad --restart
systemctl --user restart dunst.service
xrdb -merge ~/.Xresources
