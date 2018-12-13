#!/bin/bash

function start {
    $sysctl start $1
    notify-send -t 2000 "$1: Unit started"
}
function stop {
    $sysctl stop $1
    notify-send -t 2000 "$1: Unit stopped"
}

sysctl="systemctl --user"
case $1 in
    start)
	start $2
	;;
    stop)
	stop $2
	;;
    toggle)
	if [[ $(systemctl --user --value --property=ActiveState show $2) == "active" ]]; then
	    stop $2
	else
	    start $2
	fi
	;;
    status)
	notify-send -t 2500 "$($sysctl --no-pager status $2 | head -n 3 | awk '! /Loaded:/ {print $0}')"
	;;
esac

#Id,ExecMainStartTimestamp,StateChangeTimestamp,ActiveState,SubState
