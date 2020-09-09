#!/bin/sh

# install brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

## Terminal
# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
brew install zsh-syntax-highlighting zsh-completions

# spaceship theme
git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"
ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH/themes/spaceship.zsh-theme"


# Java
brew cask install adoptopenjdk
brew tap AdoptOpenJDK/openjdk
brew cask install adoptopenjdk8
brew cask install adoptopenjdk9

# install sdkman
curl -s https://get.sdkman.io | bash

# Tools
brew install git vim
brew install stow
brew install tldr


# emacs railwaycat (27.x)
# brew tap railwaycat/emacsmacport
# brew install emacs-mac --with-imagemagick --with-glib --with-modules --with-xml2 --with-ctags --with-dbus --HEAD
brew install emacs-mac --HEAD --with-jansson --with-xml2 --with-rsvg --with-modules --with-glib --with-ctags --with-dbus --with-imagemagick --with-emacs-sexy-icon

# from HEAD (28.x)
# brew tap daviderestivo/emacs-head
# brew install emacs-head --HEAD --with-cocoa --with-ctags --with-dbus --with-imagemagick --with-jansson --with-mailutils --with-no-frame-refocus --with-pdumper --with-xwidgets --with-crash-debug
# ln -s /usr/local/opt/emacs-head/Emacs.app /Applications


# ripgrep installation is in install-rust-deps.sh
brew install the_silver_searcher

# iOS
brew install carthage
gem install cocoapods --user-install

# fd (faster find)
brew install fd

# ip
brew install iproute2mac

# osquery
brew install osquery

# wireshark and tshark
brew cask install wireshark

# libvterm
brew install libvterm

# mtr
brew install mtr
sudo mkdir -p /usr/local/sbin
sudo chown $(whoami) /usr/local/sbin
brew link mtr

# sqlite3 (emacs + Dash docsets)
brew install sqlite3

# install db beaver
brew cask install dbeaver-community

# k8s
brew install kubernetes-cli
brew cask install minikube
brew install kubernetes-helm

# jq
brew install jq

# ccls
brew install ccls

# direnv
curl -sfL https://direnv.net/install.sh | bash

brew install aspell
brew install ispell

brew install fzf

# cross platform prompt written in rust
brew install starship

# podman
brew cask install podman

# Alacritty
brew cask install alacritty

# tmux
brew install tmux
brew install reattach-to-user-namespace
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# bat (better cat)
brew install bat

# nghttp2
brew install nghttp2

# mactex, gnuplot
brew cask install mactex
brew install gnuplot

# dot
brew install graphviz

# pdf-tools emac
brew install poppler automake

# git-delta
brew install git-delta

# gawk
brew install gawk

brew install gnu-time
