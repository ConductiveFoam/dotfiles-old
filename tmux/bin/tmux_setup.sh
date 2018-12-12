#!/bin/bash

if [[ -z "$TMUX" ]]; then
    dev="dev"
    web="web"

    tmux has-session -t "$dev"
    if [[ $? -ne 0 ]]; then
	tmux new-session -d -s "$dev"

#	tmux send-keys -t "$dev:1" 'clear' C-m


	tmux new-window -d -n man -t "$dev:5"

	tmux new-window -d -n mon -t "$dev:6"
	tmux split-window -v -p 60 -t "$dev:6"
	tmux send-keys -t "$dev:6.0" 'less +F /var/log/syslog' C-m
	tmux send-keys -t "$dev:6.1" 'htop' C-m
    fi
    tmux selectw -t "$dev:2"
    tmux selectw -t "$dev:1"

    tmux has-session -t "$web"
    if [[ $? -ne 0 ]]; then
	tmux new-session -d -s "$web"

	tmux send-keys -t "$web:1" 'elinks' C-m
    fi
fi
