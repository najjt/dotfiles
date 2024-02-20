#!/usr/bin/env sh

CURRENT_WIFI="$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I)"
CURR_TX="$(echo "$CURRENT_WIFI" | grep -o "lastTxRate: .*" | sed 's/^lastTxRate: //')"
SSID="$(networksetup -getairportnetwork en0 | awk -F': ' '{print $2}' | tail -n 1)"

if [ "$CURR_TX" = "" ]; then
    sketchybar --set $NAME label="disconnected"
else
    sketchybar --set $NAME label="| ${SSID}"
fi
