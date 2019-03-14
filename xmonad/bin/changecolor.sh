#!/usr/bin/bash

# Modify .Xresources
sed -e 's/: S_base0\([[:digit:]]\)$/: S_base0\1;/g' \
    -e 's/: S_base\([[:digit:]]\)$/: S_base0\1/g' \
    -e 's/: S_base0\([[:digit:]]\);$/: S_base\1/g' \
    --follow-symlinks -i ~/.Xresources
xrdb -merge ~/.Xresources

replace_variable_in_file() {
    # replace_variable_in_file fn sep var a b
    # Assumes a certain string is not present in the file!
    replace_key="0DlfGVGZ1a"
    sed -e "s/$3$2$4/${replace_key}/g" \
	-e "s/$3$2$5/$3$2$4/g" \
	-e "s/${replace_key}/$3$2$5/g" \
	--follow-symlinks -i "$1"
}

getcolumn() {
    echo $1 | cut -d';' -f$2
}

# Files with two variables to replace values of
# Replace fore- and background colors
quoted_hex_colors_bg='"#002b36";"#fdf6e3"'
quoted_hex_colors_fg='"#839496";"#657b83"'
hex_colors_bg='#002b36;#fdf6e3'
hex_colors_fg='#839496;#657b83'
hex_colors_cursor='#93a1a1;#073642'
file_replacements=(
    #"/tmp/replacetest; = ;colBackground;colLBlack;colLWhite;background;${quoted_hex_colors_bg}"
    # XMonad-related
    "$HOME/.xmonad/lib/MyColors.hs; = ;colBackground;colLBlack;colLWhite;colForeground;colLBlue;colLYellow"
    "$HOME/.xmobarrc; = ;bgColor;${quoted_hex_colors_bg};fgColor;${quoted_hex_colors_fg}"
    "$HOME/.config/dunst/dunstrc;background; = ;${quoted_hex_colors_bg};frame_color;${quoted_hex_colors_fg}"

    # Terminals
    "$HOME/.config/xfce4/terminal/terminalrc;=;ColorBackground;${hex_colors_bg};ColorForeground;${hex_colors_fg}"
    "$HOME/.config/xfce4/terminal/terminalrc;=;ColorCursor;${hex_colors_cursor};ColorBold;${hex_colors_cursor}"
    "$HOME/dotfiles/terminals/.config/alacritty/alacritty.yml;: ;background;'0x002b36';'0xfdf6e3';foreground;'0x839496';'0x657b83'"
)
for row in "${file_replacements[@]}"; do
    if [ -f "$(getcolumn "$row" 1)" ]; then
	replace_variable_in_file "$(getcolumn "$row" 1)" "$(getcolumn "$row" 2)" \
				 "$(getcolumn "$row" 3)" "$(getcolumn "$row" 4)" "$(getcolumn "$row" 5)"
	replace_variable_in_file "$(getcolumn "$row" 1)" "$(getcolumn "$row" 2)" \
				 "$(getcolumn "$row" 6)" "$(getcolumn "$row" 7)" "$(getcolumn "$row" 8)"
    fi
done

# XMonad restart: Kill tray, restart xmonad and dunst
pkill stalonetray
systemctl --user restart dunst.service 2> /dev/null
xmonad --recompile 2> /dev/null && xmonad --restart

# Exchange specific face color
dotemacs="$HOME/.emacs.d/init.el"
if [ -f "$dotemacs" ]; then
    sed -e 's/((t (:foreground "black")))/((t isblack))/g' \
	-e 's/((t (:foreground "white")))/((t (:foreground "black")))/g' \
	-e 's/((t isblack))/((t (:foreground "white")))/g' \
	--follow-symlinks -i "$dotemacs"
fi
