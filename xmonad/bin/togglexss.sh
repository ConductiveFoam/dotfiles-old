#!/bin/bash

mode=$( gawk '/^mode/{print $2}' .xscreensaver )
case $mode in
    off)
	mode=one
	;;
    one)
	mode=off
	;;
    *)
	mode=one
	;;
esac

sed -i "s/\\(mode:\t\t\\).*/\\1$mode/" .xscreensaver
