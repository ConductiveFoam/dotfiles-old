# ~/.bash_profile
#

# PATH setup
PATH="$HOME/.local/bin:$PATH"
# Programming languages
#  Rust
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
PATH="$HOME/.cargo/bin:$PATH"
#  Haskell
PATH="$HOME/.cabal/bin:$PATH"
#  Go
export GOPATH="$HOME/.go"
PATH="$GOPATH/bin:$PATH"
#  Ruby
PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
#  Awk
export AWKPATH=".:$HOME/.local/share/awk:/usr/local/share/awk"

# Priority: ~/bin, language-specific bins, ~/.local/bin, then others
export PATH="$HOME/bin:$PATH"

export AURDEST="$HOME/build"

if [ -d ~/.profile.d ]; then
  for fn in ~/.profile.d/*; do
    [ -x "$fn" ] && . "$fn"
  done
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  startx
fi
