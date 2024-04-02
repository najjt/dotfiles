#!/usr/bin/env sh

SSID="$(networksetup -getairportnetwork en0 | awk -F': ' '{print $2}' | tail -n 1)"
LABEL=""

if [[ $SSID = "" ]]; then
   LABEL="disconnected"
else
   LABEL="${SSID}"
fi

sketchybar --set $NAME label="| ${LABEL}"
