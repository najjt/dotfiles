#!/usr/bin/env sh

sketchybar  --add item date_and_time right                                 \
            --set      date_and_time update_freq=5                         \
                                     script="$PLUGIN_DIR/date_and_time.sh" \
