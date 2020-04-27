#!/bin/bash


open -a XQuartz
ip=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
xhost + $ip

docker run --rm -ti --network host -e DISPLAY=$ip:0 -v /tmp/.X11-unix:/tmp/.X11-unix andreacorallo/emacs-nativecomp emacs

