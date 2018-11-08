#!/bin/bash
mode=$(gawk '/^mode/ {print $2}' .xscreensaver)
if [[ "$mode" = "one" ]]; then
    echo "<fc=#859900>on</fc> "
else
    echo "<fc=#dc322f>$mode</fc>"
fi
