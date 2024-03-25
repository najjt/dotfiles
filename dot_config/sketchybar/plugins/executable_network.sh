#!/usr/bin/env sh

SSID="$(networksetup -getairportnetwork en0 | awk -F': ' '{print $2}' | tail -n 1)"

sketchybar --set $NAME label="| ${SSID}"
