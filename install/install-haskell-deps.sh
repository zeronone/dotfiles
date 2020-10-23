#!/bin/sh

mkdir -p ~/.ghcup/bin
curl https://raw.githubusercontent.com/haskell/ghcup/master/ghcup > ~/.ghcup/bin/ghcup
chmod +x ~/.ghcup/bin/ghcup

ghcup install latest
ghcup install-cabal latest
ghcup set latest
# upgrade to latest
cabal update
cabal new-install cabal-install
