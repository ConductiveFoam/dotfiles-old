#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias lsa='ls -a'
alias lsl='ls -l'
alias lsal='ls -al'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias cd..='cd ..'
alias cd...='cd ../..'
alias cd~.='cd ~/dotfiles'
alias rmi='rm -i'

alias sysdctl='systemctl --user'

# Setting HOME for this does undesired things, e.g. save .cache there, so we're working around
alias xscreensaver-demo="xscreensaver-demo; mv ~/.xscreensaver ~/dotfiles/xconfig/.xscreensaver; ln -sf ~/dotfiles/xconfig/.xscreensaver ~"

ssht() { /usr/bin/ssh -t "$@" "tmux attach || tmux new"; }

PS1='[\u@\h \W]\$ '

export EDITOR="emacsclient -nw"
export VISUAL="emacsclient"

PATH="$HOME/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$PATH"
export PATH

export BUILDDIR="$HOME/build"
