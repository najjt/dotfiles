#!/usr/bin/env sh

IS_VPN=$(ifconfig | grep -m1 'utun4')

if [[ $IS_VPN != "" ]]; then
    LABEL="ON"
else
    LABEL="OFF"
fi

sketchybar --set $NAME label="VPN: ${LABEL}"
