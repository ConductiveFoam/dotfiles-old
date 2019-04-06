#!/bin/bash
fmt='@include "xmobar_lib.awk"

BEGIN { tempstr = "" }
/^Core/ {
  temp = substr($3, 2, index($3, ".") - 2)
  if ($2 == "0:") {
    tempstr = tempstr format(50, 70, temp)
  } else {
    tempstr = tempstr ":" format(50, 70, temp)
  }
}
/^[1-9][0-9]*$/ {
  tempstr = tempstr ";" format(50, 70, $0)
}
END { print tempstr }'

echo "$(sensors | cat - <(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader) | awk "$fmt")°C"
