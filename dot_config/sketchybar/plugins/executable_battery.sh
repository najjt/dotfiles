#!/usr/bin/env sh

PERCENTAGE=$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)
TIME=$(pmset -g batt | grep -Eo "\d+:\d+" | cut -d% -f1)
LABEL="${PERCENTAGE}% $TIME"

if [ $PERCENTAGE = "" ]; then
    exit 0
fi

sketchybar --set $NAME label="| BAT: $LABEL"
