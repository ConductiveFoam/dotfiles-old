#!/bin/bash
fmt='function formattemp(c) {
  if (70 < c) {
    col = "#dc322f"
  } else if (50 < c) {
    col = "#859900"
  } else {
    col = "#268bd2"
  }
  return "<fc=" col ">" c "</fc>"
}

BEGIN { tempstr = "" }
/^Core/ {
  temp = substr($3, 2, index($3, ".") - 2)
  if ($2 == "0:") {
    tempstr = tempstr formattemp(temp)
  } else {
    tempstr = tempstr "|" formattemp(temp)
  }
}
/^[1-9][0-9]*$/ {
  tempstr = tempstr ";" formattemp($0)
}
END { print tempstr }'

echo "$(sensors | cat - <(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader) | awk "$fmt")°C"
