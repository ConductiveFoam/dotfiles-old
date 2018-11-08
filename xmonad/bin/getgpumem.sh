#! /bin/bash

function formatmem() {
    if [[ "$1" -gt "50" ]]; then
	col="#dc322f"
    elif [[ "$1" -gt "15" ]]; then
	col="#859900"
    else
	col="#268bd2"
    fi
    echo "<fc=$col>$1</fc>"
}

echo ";$(formatmem $(nvidia-smi --query-gpu=memory.used,memory.total --format=csv | tail -n1 | awk '{print int(($1 / $3) * 100)}'))%"
