#!/usr/bin/env sh

sketchybar --add item vpn right                       \
           --set      vpn script="$PLUGIN_DIR/vpn.sh" \
                          update_freq=30
