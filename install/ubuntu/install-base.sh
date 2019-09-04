#!/bin/sh

sudo apt update
sudo apt install xsel xclip -y
sudo apt install silversearcher-ag -y
sudo apt install git vim curl wget zsh -y
sudo apt install stow -y
sudo apt install make -y
sudo apt install texinfo -y
sudo apt install gcc g++ -y

# emacs
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt update
sudo apt install emacs26 -y

sudo rm /usr/bin/emacs
sudo ln -s /usr/bin/emacs26 /usr/bin/emacs

# java
sudo apt install default-jdk -y
sudo apt install openjdk-8-jdk -y

# libvterm
sudo apt install libvterm-dev


