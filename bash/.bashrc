#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LS_COLORS='ln=00;36'

if [ -d ~/.shrc.d ]; then
  for fn in ~/.shrc.d/*; do
    . "$fn"
  done
fi

eval $(thefuck --alias)

PS1='[\u@\h \W]\$ '
