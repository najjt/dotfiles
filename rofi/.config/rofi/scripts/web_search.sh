#!/bin/bash

if [ -z "$ROFI_RETV" ]; then
    echo "Type your Google search query and press Enter"
    exit 0
fi

if [ "$ROFI_RETV" -eq 0 ]; then
    exit 0
fi

QUERY="$1"

if [[ -n "$QUERY" ]]; then
    xdg-open "https://www.google.com/search?q=$(echo "$QUERY" | sed 's/ /+/g')" >/dev/null 2>&1 &
    disown
fi
