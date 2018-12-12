#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

ssht() { /usr/bin/ssh -t "$@" "tmux attach || tmux new"; }

PS1='[\u@\h \W]\$ '

export EDITOR="emacsclient -nw"
export VISUAL="emacsclient"

PATH="$HOME/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$PATH"
export PATH
