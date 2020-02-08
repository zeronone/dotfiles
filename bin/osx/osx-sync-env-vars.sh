#!/bin/zsh

set -x

for i in $(export); do
	var=$(echo $i|sed 's/=.*//')
	val=$(echo $i|sed 's/^[^=]*=//')
	[[ $val != "" ]] && {
		launchctl setenv $var $val
    }
done

