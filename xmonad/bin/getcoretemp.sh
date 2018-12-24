#!/bin/bash

function coretemp() {
    temp=$(sensors | gawk "/Core $1/{\$3=substr(\$3, 2, 2); print \$3}")
    echo $(formattemp $temp)
}

function gputemp() {
    temp=$(nvidia-smi --query-gpu=temperature.gpu --format=csv | tail -n1)
    echo $(formattemp $temp)
}

function formattemp() {
    if [[ "$1" -gt "70" ]]; then
	col="#dc322f"
    elif [[ "$1" -gt "50" ]]; then
	col="#859900"
    else
	col="#268bd2"
    fi
    echo "<fc=$col>$1</fc>"
}

c0=$(coretemp 0)
c1=$(coretemp 1)
#c2=$(coretemp 2)
#c3=$(coretemp 3)

g=$(gputemp)

echo "${c0}|${c1}C"
# g°
