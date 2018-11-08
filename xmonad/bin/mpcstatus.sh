#!/bin/bash

function get_state {
    mpc status | awk "/^volume/ {print \$$1}"
}

col="#839496"
l="["
r="]"
m=" "
p=" "

s=$(mpc status)
if [[ $? != 0 ]]; then
    col="#dc322f"
    m="X"
else
    if (( $(mpc playlist | wc -l) > 0 )); then
	p="+"
    fi
    if [[ "$(get_state 4)" == "on" ]]; then
	l="R"
    fi
    if [[ "$(get_state 6)" == "on" ]]; then
	r="S"
    fi

    case $(mpc status | head -n1) in
	volum*)
	;;
	*)
	    case $(mpc status | head -n2 | tail -n1) in
		"[playing]"*)
		    col="#859900"
		    m="P"
		    ;;
		*)
		    col="#268bd2"
		    m="N"
	    esac
    esac
fi
echo "<fc=$col>${p}${l}${m}${r}</fc>"
