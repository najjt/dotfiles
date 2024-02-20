#!/usr/bin/env sh

sketchybar --add item network right                           \
           --set      network update_freq=4                   \
                              script="$PLUGIN_DIR/network.sh" \
