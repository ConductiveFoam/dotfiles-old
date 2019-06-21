# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -d ~/.profile.d ]; then
  for fn in ~/.profile.d/*; do
    [ -x "$fn" ] && . "$fn"
  done
fi

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  startx
fi
