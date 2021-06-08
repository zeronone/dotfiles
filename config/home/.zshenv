
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

### Ruby Gems
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH

# Golang
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin


# dotfiles common bin
export PATH=$PATH:$HOME/myfiles/dotfiles/bin
export PATH=$HOME/bin:$PATH


## BAT
export BAT_THEME="Monokai Extended Light"

## Java
export MAVEN_OPTS="-Dmaven.artifact.threads=30"
. "$HOME/.cargo/env"
