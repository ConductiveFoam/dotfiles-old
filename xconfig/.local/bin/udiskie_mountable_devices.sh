#! /usr/bin/bash
devices=$(udiskie-info -a)
for fn in $devices; do
    # If the device shows up in lsblk as encrypted it's already mounted
    [[ -z `lsblk -n "$fn" 2> /dev/null | grep crypt` ]] && echo "$fn"
done
