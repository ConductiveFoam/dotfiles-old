#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LS_COLORS='ln=00;36'

# Convenience aliases
alias ls='ls --color=auto'
alias lsa='ls -a'
alias lsl='ls -l'
alias lsal='ls -al'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias cd.='cd ~/dotfiles'
alias ..='cd ..'
alias ...='cd ../..'
alias rmi='rm -i'

alias clr='clear -x'

alias git?='clr; git status'
alias git??='git?; git stash list; git --no-pager log -3 --format=shortdate'
alias git-amend='git commit --amend --no-edit'

alias sysdctl='systemctl --user'

alias ncdu='ncdu -2 -rr --confirm-quit'

# Need to manually move and relink config since xscreensaver-demo does not respect $HOME as location for .xscreensaver
XSCREENSAVER_CONFIG="$HOME/dotfiles/xconfig/.xscreensaver"
alias xscreensaver-demo="xscreensaver-demo; mv ~/.xscreensaver $XSCREENSAVER_CONFIG; ln -sf $XSCREENSAVER_CONFIG ~"

# Fortune aliases
alias koan='fortune koan'
alias tao='fortune tao'
alias pratchett='fortune pratchett'
alias empty=' clr; fortune pratchett tao koan'

eval $(thefuck --alias)

ssht() { /usr/bin/ssh -t "$@" "tmux attach || tmux new"; }

# Programming languages
#  Rust
alias clippy='cargo +stable clippy'
alias rustfmt='cargo +nightly fmt'

PS1='[\u@\h \W]\$ '

export EDITOR="emacsclient -nw"
export VISUAL="emacsclient"
export SUDO_EDITOR="emacs -nw"
