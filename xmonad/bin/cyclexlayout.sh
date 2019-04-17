#!/bin/bash

layout=$(setxkbmap -query | gawk '/layout/ {print $2}')
case $layout in
    us)
	layout=dvorak
	;;
    de)
	layout=us
	;;
    *)
	layout=de
	;;
esac

setkeyboard.sh "$layout"
