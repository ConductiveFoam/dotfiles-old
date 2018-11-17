#!/bin/bash
mode=$(systemctl --user --property=ActiveState show xss-deactivate.timer)

case $mode in
    ActiveState=inactive)
	cmd=start
	;;
    ActiveState=active)
	cmd=stop
	;;
    *)
	cmd=start
	;;
esac

systemctl --user $cmd xss-deactivate.timer
