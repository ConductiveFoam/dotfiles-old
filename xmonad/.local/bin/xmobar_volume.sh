#!/bin/bash
format='NR == 6 {
  volume = substr($5, 2, length($5) - 3)
  if ($6 == "[on]") {
    color = "#859900"
    status = "on "
  } else {
    color = "#dc322f"
    status = "off"
  }
  print "Vol: <fc=#268bd2>" volume "</fc>% <fc=" color ">" status "</fc>"
}
'

echo "$(amixer get Master | awk "$format")"
