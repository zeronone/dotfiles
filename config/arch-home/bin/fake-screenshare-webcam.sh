#!/bin/bash

sudo modprobe v4l2loopback

wf-recorder --muxer=v4l2 --codec=rawvideo --pixel-format=yuv420p --file=/dev/video2
