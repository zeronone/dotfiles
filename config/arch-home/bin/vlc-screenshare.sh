#!/bin/sh

# Screen sharing for legacy applications on wayland
# works by playing a stream of the selected display using
# VLC (on XWayland) to a headless display. The VLC will
# be recognized by X screen sharing software and can be
# used to share the output selected earlier

# create virtual display
if ! [[ "$(swaymsg -t get_outputs -r | jq '.[] | select(.name == "HEADLESS-1")')" ]]; then
    echo "Creating headless output...."
    # create workspace "share" that will be on the created output
    # assuming that user does not have a workspace called "share"
    # and thus can't focus it accidentially
    swaymsg "workspace share output HEADLESS-1"
    swaymsg "create_output"

    # move output far away so it can't be focused by the mouse
    swaymsg "output HEADLESS-1 pos 5000 5000"
fi

export BEMENU_BACKEND=curses
OUTPUT="$(swaymsg -t get_outputs | jq -r '.[] | select(.name != "HEADLESS-1") | .name + ": " + .make + " " + .model' | bemenu | awk -F: '{print $1}' || exit 1)"
echo "Selected output: $OUTPUT"

trap 'kill $(jobs -p); rm /tmp/screen.avi' EXIT
mkfifo /tmp/screen.avi
cvlc --fullscreen --play-and-exit /tmp/screen.avi &
sleep .5
wf-recorder -o $OUTPUT -f /tmp/screen.avi &
while ! swaymsg '[title="VLC media player"] move to output HEADLESS-1'; do sleep .1; done
wait
