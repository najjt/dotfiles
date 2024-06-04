#!/usr/bin/env sh

PERCENTAGE=$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)
TIME=$(pmset -g batt | grep -Eo "\d+:\d+" | cut -d% -f1)
CHARGING=$(pmset -g batt | grep -Eo "\w*charging" | cut -d% -f1)
LABEL="${PERCENTAGE}%"

if [ $PERCENTAGE = "" ]; then
    exit 0
fi

if [ $CHARGING = "discharging" ]; then
    LABEL="${PERCENTAGE}% $TIME"
elif [ $CHARGING = "charging" ]; then
    LABEL="${PERCENTAGE}% (charging)"
fi

sketchybar --set $NAME label="| BAT: $LABEL"
