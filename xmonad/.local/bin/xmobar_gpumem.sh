#! /bin/bash
fmt='@include "xmobar_lib.awk"
{ print format(15, 50, int(($1 / $3) * 100))}'

echo ";$(nvidia-smi --query-gpu=memory.used,memory.total --format=csv,noheader | awk "$fmt")%"
