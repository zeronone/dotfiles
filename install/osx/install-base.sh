#!/bin/sh

# install brew
# And add to ~/.zshrc
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"


## Terminal
# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-autosuggestions "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions"
brew install zsh-syntax-highlighting zsh-completions

# spaceship theme
git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"
ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH/themes/spaceship.zsh-theme"

# Java
#brew cask install adoptopenjdk
#brew tap AdoptOpenJDK/openjdk
#brew cask install adoptopenjdk8
#brew cask install adoptopenjdk9

# install sdkman
#curl -s https://get.sdkman.io | bash

# install fonts
brew tap homebrew/cask-fonts
brew install --cask font-iosevka
brew install --cask font-cascadia-code-pl


# Tools
brew install git vim
brew install stow
brew install tldr

# emacs
brew tap jimeh/emacs-builds
brew install --cask emacs-app

# ripgrep installation is in install-rust-deps.sh
brew install the_silver_searcher

# fd (faster find)
# brew install fd

# ip
brew install iproute2mac

# libvterm
brew install libvterm

# jq
brew install jq

# direnv
curl -sfL https://direnv.net/install.sh | bash

brew install fzf

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

# ccls
brew install ccls

# editorconfig
brew install editorconfig

# hammerspoon
# brew install hammerspoon --cask

# cmake
brew install cmake

