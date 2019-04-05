#!/bin/bash
format='
BEGIN {
  mpdstate = " "
  playstate = ""
  track = ""
  col = "#268bd2"
}
NR == 1 {
  track = $0
}
/^\[[^]]+\]/ {
  if ($1 == "[playing]") {
    col = "#859900"
  }
  playstate = $2 " * " $3
}
/^volume/ && NR != 1 {
  if ($4 == "on") {
    mpdstate = mpdstate "R"
  }
  if ($6 == "on") {
    mpdstate = mpdstate "S"
  }
  if ($8 == "on") {
    mpdstate = mpdstate "O"
  }
  if ($10 == "on") {
    mpdstate = mpdstate "C"
  }
}
END {
  if (NR == 1) {
    print "<fc=" col ">No playlist loaded</fc>"
  } else {
    print mpdstate " | " playstate " | <fc=" col ">" track "</fc>"
  }
}'

_=$(mpc status)
if [[ $? != 0 ]]; then
    echo "<fc=#dc322f>MPD not running</fc>"
else
    titleformat="[[%artist% -- %album% -- ]%title%]|[file: %file%]"
    echo "$(mpc status -f "$titleformat" | awk "$format")"
fi
