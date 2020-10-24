#!/bin/sh

# might need to install sudo netctl beforehand

sudo pacman -S --noconfirm \
	netctl \
	dhcpcd \
	stow \
	man \
	vim \
	git \
	sway \
	swaylock \
	swayidle \
	rofi \
	dmenu \
	alacritty \
	zsh \
	curl \
	wget \
	adobe-source-han-sans-jp-fonts \
	adobe-source-han-serif-jp-fonts \
	otf-ipafont \
	ttf-hanazono \
	ttf-sazanami \
	ttf-fira-code \
	ttf-fira-mono \
	otf-fira-mono \
	ttf-hack \
	noto-fonts \
	htop \
	openssh \
	cmake \
	i3status-rust \
	ttf-font-awesome \
	redshift \
	firefox \
	aspell \
	aspell-en \
	shellcheck \
	jdk-openjdk \
	openjdk-doc \
	openjdk-src \
	iwd \
	ethtool \
	editorconfig-core-c \
	pandoc \
	graphviz \
	go \
	pyenv \
	sbcl \
	ccls \
	glslang \
	discord \
	ripgrep \
	sqlite


# AUR Repositories
sudo pacman -S --needed --noconfirm base-devel


