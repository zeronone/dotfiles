#!/bin/sh

# Electron with wayland support

# electron-ozone provides "electron" which can be used with electron apps
#yay -S openh264
#yay -S --noconfirm \
#	electron-ozone
#
#yay -S --noconfirm \
#	slack-electron
#
## code needs to be edited : Replace electron9 with electron
#yay -S --editmenu \
#	code

# xorg-server-hidpi-git
# https://gitlab.freedesktop.org/xorg/xserver/-/merge_requests/432
yay -S xorg-server-hidpi-git wlroots-hidpi-git sway-hidpi-git


yay -S fedora-firefox-wayland-bin


