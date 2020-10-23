
# The ones defined later have higher priority

SHELL=/bin/zsh

### PATH
export PATH=/usr/local/sbin:$PATH
# for haskell stack local bins
export PATH=$HOME/.local/bin:$PATH   # 3
export PATH=$HOME/.ghcup/bin:$PATH   # 2
export PATH=$HOME/.cabal/bin:$PATH   # 1
# for rustup, cargo installations
export PATH=$HOME/.cargo/bin:$PATH


# doom emacs
export PATH=$HOME/.emacs.d/bin:$PATH

# shell inside emacs
# https://github.com/ohmyzsh/ohmyzsh/issues/6411
export EMACS="*term*"


# Include scripts folder
export PATH=$HOME/scripts:$PATH

# RUST_SRC_PATH
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

### Ruby Gems
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH

# Golang
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# brew install bintuils
export PATH="/usr/local/opt/binutils/bin:$PATH"

# manpth
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH-/usr/share/man}"
export MANPATH="$HOME/man/linux-kernel:${MANPATH-/usr/share/man}"
export PATH="/usr/local/opt/curl/bin:$PATH"

# dotfiles common bin
export PATH=$PATH:$HOME/myfiles/dotfiles/bin
export PATH=$HOME/bin:$PATH


## llvm (clangd)
export PATH="/usr/local/opt/llvm/bin:$PATH"

## BAT
export BAT_THEME="Monokai Extended Light"

## Java
export MAVEN_OPTS="-Dmaven.artifact.threads=30"
