#!/bin/sh

brew install stack
mkdir -p ~/.ghcup/bin
curl https://raw.githubusercontent.com/haskell/ghcup/master/ghcup > ~/.ghcup/bin/ghcup
chmod +x ~/.ghcup/bin/ghcup

stack setup

ghcup install latest
ghcup install-cabal latest
ghcup set latest
# upgrade to latest
cabal update
cabal new-install cabal-install

# mkdir -p ~/bin
# git clone https://github.com/haskell/haskell-ide-engine.git ~/bin/haskell-ide-engine --recursive
# cd ~/bin/haskell-ide-engine
# stack ./install.sh build-all


