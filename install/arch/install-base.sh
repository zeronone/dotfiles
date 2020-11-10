#!/bin/sh

# might need to install sudo netctl beforehand

sudo pacman -Syu --noconfirm \
	netctl \
	dhcpcd \
	stow \
	man \
	vim \
	git \
	swaylock \
	swayidle \
	rofi \
	dmenu \
	alacritty \
	zsh \
	curl \
	wget \
	adobe-source-han-sans-otc-fonts \
	adobe-source-han-serif-otc-fonts \
	adobe-source-han-sans-jp-fonts \
	adobe-source-han-serif-jp-fonts \
	adobe-source-han-sans-kr-fonts \
	otf-ipafont \
	ttf-hanazono \
	ttf-sazanami \
	ttf-fira-code \
	ttf-fira-mono \
	otf-fira-mono \
	ttf-hack \
	noto-fonts \
	noto-fonts-cjk \
	ttf-baekmuk \
	htop \
	openssh \
	cmake \
	ttf-font-awesome \
	redshift \
	aspell \
	aspell-en \
	jdk-openjdk \
	openjdk-doc \
	openjdk-src \
	iwd \
	ethtool \
	editorconfig-core-c \
	graphviz \
	go \
	pyenv \
	sbcl \
	ccls \
	glslang \
	discord \
	ripgrep \
	sqlite \
	usbutils \
	vlc \
	fprintd \
	imagemagick \
	wmname \
	qt5-wayland \
	alsa-utils \
	bluez-utils \
	fcitx-im \
	fcitx-configtool \
	fcitx-mozc \
	waybar \
	cantarell-fonts \
	awesome-terminal-fonts \
	brightnessctl \
	light \
	intellij-idea-community-edition \
	code \
	pulseaudio \
	pipewire pipewire-alsa pipewire-pulse pipewire-jack pipewire-docs \
	mako \
	grim \
	slurp \
	xdg-desktop-portal-wlr \
	xdg-desktop-portal \
	libpipewire02 \
	wf-recorder \
	jq \
	xdg-user-dirs \
	noto-fonts-emoji \
	chromium \
	eog \
	linux-headers \
	qt5-webengine v4l2loopback-dkms \
	bat

#	sway \
#	xorg-server-xwayland \

# zoom
# https://hugo.barrera.io/journal/2020/06/14/zoom-screensharing-on-archlinux/

# we need to setuid for light
# sudo usermod -aG video arif
# sudo chmod +s /usr/bin/light

# AUR Repositories
sudo pacman -S --needed --noconfirm base-devel


# Following should be disabled, started from sway
#   systemctl --user disable pipewire.socket pipewire.service
#   systemctl --user disable xdg-desktop-portal xdg-desktop-portal-gtk xdg-desktop-wlr

