#!/bin/bash
mode=$(systemctl --user --property=ActiveState show xss-deactivate.timer)
if [[ "$mode" = "ActiveState=inactive" ]]; then
    echo "<fc=#859900>on</fc> "
else
    echo "<fc=#dc322f>off</fc>"
fi
