#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LS_COLORS='ln=00;36'

# Explicit XDG spec to make other things a bit easier
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Convenience aliases
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
alias git??='git?; git stash list; git log -3 --format="format:%C(yellow)commit %H%C(auto)% d%n%CresetDate:   %aD%n%s%n"'
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
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
PATH="$HOME/.cargo/bin:$PATH"
#  Haskell
PATH="$HOME/.cabal/bin:$PATH"
#  Go
export GOPATH="$HOME/.go"
PATH="$GOPATH/bin:$PATH"
#  Ruby
PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"

PS1='[\u@\h \W]\$ '

export EDITOR="emacsclient -nw"
export VISUAL="emacsclient"
export SUDO_EDITOR="emacs -nw"

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export AWKPATH=".:$HOME/.local/share/awk:/usr/local/share/awk"

export BUILDDIR="$HOME/build"
