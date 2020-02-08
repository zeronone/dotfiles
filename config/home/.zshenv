
# The ones defined later have higher priority

SHELL=/bin/zsh

### PATH
export PATH=$HOME/bin:$PATH
export PATH=/usr/local/sbin:$PATH
# for haskell stack local bins
export PATH=$HOME/.local/bin:$PATH   # 3
export PATH=$HOME/.ghcup/bin:$PATH   # 2
export PATH=$HOME/.cabal/bin:$PATH   # 1
# for rustup, cargo installations
export PATH=$HOME/.cargo/bin:$PATH

# doom emacs
export PATH=$HOME/.emacs.d/bin:$PATH

# coreutils
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# Include scripts folder
export PATH=$HOME/scripts:$PATH

export ANDROID_HOME=/Users/$USER/Library/Android/sdk
export PATH=${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools


# RUST_SRC_PATH
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

### Ruby Gems
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH

# Golang
export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=$PATH:$GOPATH/bin

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#export SDKMAN_DIR="$HOME/.sdkman"
#[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# brew install bintuils
export PATH="/usr/local/opt/binutils/bin:$PATH"

# go version manager (gvm)
source $HOME/.gvm/scripts/gvm

# manpth
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH-/usr/share/man}"
export MANPATH="$HOME/man/linux-kernel:${MANPATH-/usr/share/man}"
export PATH="/usr/local/opt/curl/bin:$PATH"

# dotfiles common bin
export PATH=$PATH:$HOME/myfiles/dotfiles/bin
