#!/bin/sh

# electron-ozone provides "electron" which can be used with electron apps
yay -S --noconfirm \
	electron-ozone

yay -S --noconfirm \
	slack-electron

# code needs to be edited : Replace electron9 with electron
yay -S --editmenu \
	code
