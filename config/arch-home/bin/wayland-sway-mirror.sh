#!/bin/bash

# https://gist.io/@progandy/bff675311aa2c3b777a37abe81aa4b4d

# sway-mirror OPTIONS
#
# Examples:
#
# sway-mirror
#
# sway-mirror --output LVDS-1
#
export SDL_VIDEODRIVER=wayland
instance=sway-mirror$$
wf-recorder "$@" -c rawvideo -m sdl -f pipe:$instance &
sleep 1
if kill -0 $! 2>/dev/null && swaymsg "[title=\"^pipe:$instance\$\"] nop" >/dev/null; then
    while read type ; read name ; do 
        if [[ $type == '"close"' && $name == "\"pipe:$instance\"" ]]; then
            break
        fi
    done < <(swaymsg -mt subscribe '["window"]' | jq --unbuffered .change,.container.name)
    #while jobs '%1' >/dev/null && swaymsg "[title=\"^pipe:wf-mirror$$\$\"] nop" >/dev/null; do
    #    sleep 1
    #done
fi
kill -INT '%1'
exit 0
