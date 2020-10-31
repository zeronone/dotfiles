#!/bin/bash

export MODULE="v4l2loopback"
modprobe -n --first-time $MODULE && echo "v4l2loopback not loaded, loading" || echo "Loaded"

wf-recorder --muxer=v4l2 --codec=rawvideo --pixel-format=yuv420p --file=/dev/video2
