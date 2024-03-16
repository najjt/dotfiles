#!/usr/bin/env sh

DATE=$(date '+%a %d %b')
TIME=$(date '+%H:%M')

sketchybar --set $NAME label="| $DATE $TIME"
