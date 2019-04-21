#!/usr/bin/bash
if [ -n "$3" ]; then
    if [ $2 -eq 1 ]; then
	notify-send -t 5000 "Finished download of $3"
    else
	notify-send -t 5000 "Finished $2 downloads in $(dirname $3)"
    fi
fi
