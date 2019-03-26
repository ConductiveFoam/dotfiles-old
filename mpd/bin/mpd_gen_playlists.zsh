#!/usr/bin/zsh
zmodload zsh/mapfile
pathprefix="$(gawk "/^music_directory/ {gsub(\"~\", \"$HOME\"); print substr(\$2, 2, length(\$2) - 2);}" ~/.config/mpd/mpd.conf)"
cd $pathprefix
excludes=(${(f)mapfile[mpd_exclude.txt]})
pls=(${(f)"$(mpc lsplaylists)"})

for fn in *; do
    # Skip files and undesired folders
    if [[ -f "$fn" ]] || [[ ${excludes[(i)$fn]} -le ${#excludes} ]] {
        continue
    }
    # Skip already existing playlists
    if [[ ${pls[(i)l$fn]} -le ${#pls} ]] {
        continue
    }

    mpc clear
    mpc ls "$fn" | mpc add
    mpc save "$fn"
done
