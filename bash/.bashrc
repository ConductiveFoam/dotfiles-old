#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LS_COLORS='ln=00;36'

alias ls='ls --color=auto'
alias lsa='ls -a'
alias lsl='ls -l'
alias lsal='ls -al'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias cd.='cd ~/dotfiles'
alias cd..='cd ..'
alias cd...='cd ../..'
alias rmi='rm -i'

alias clr='clear'

alias git?='clear; git status'

alias sysdctl='systemctl --user'

alias ncdu='ncdu -2 -rr --confirm-quit'

# Setting HOME for this does undesired things, e.g. save .cache there, so we're working around
alias xscreensaver-demo="xscreensaver-demo; mv ~/.xscreensaver ~/dotfiles/xconfig/.xscreensaver; ln -sf ~/dotfiles/xconfig/.xscreensaver ~"

# Fortune aliases
alias koan='fortune koan'
alias tao='fortune tao'
alias pratchett='fortune pratchett'
alias empty=' clr; fortune pratchett tao koan'

eval $(thefuck --alias)

ssht() { /usr/bin/ssh -t "$@" "tmux attach || tmux new"; }

PS1='[\u@\h \W]\$ '

export EDITOR="emacsclient -nw"
export VISUAL="emacsclient"

PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$PATH"
export PATH
export AWKPATH=".:$HOME/.local/share/awk:/usr/local/share/awk"

export BUILDDIR="$HOME/build"
