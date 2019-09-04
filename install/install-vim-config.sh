#!/bin/sh

cd $HOME
git clone https://github.com/jessfraz/.vim $HOME/.vim --recursive
cd $HOME/.vim
git submodule sync --recursive && git submodule update --init --recursive

