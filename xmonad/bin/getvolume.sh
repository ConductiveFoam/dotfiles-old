#!/bin/bash

function getcolumn() {
    amixer get Master | head -n6 | tail -n1 | cut -d' ' -f$1
}

vol=$(getcolumn 7 | grep -o '[0-9]*')
status=$(getcolumn 8)
if [ "$status" == "[on]" ]; then
    color="#859900"
    status="$status "
else
    color="#dc322f"
fi

echo "Vol: <fc=#268bd2>${vol}</fc>% <fc=${color}>${status}</fc> "
