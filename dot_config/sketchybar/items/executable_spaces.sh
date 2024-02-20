#!/usr/bin/env sh

SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")

for i in "${!SPACE_ICONS[@]}"
do
  sid=$(($i+1))
  sketchybar --add space space.$sid left                         \
             --set space.$sid space=$sid                         \
                              label=${SPACE_ICONS[i]}            \
                              background.color=0xff284764        \
                              background.border_color=0xff405D76 \
                              background.border_width=1          \
                              background.height=24               \
                              background.padding_right=1         \
                              background.padding_left=1          \
                              label.padding_right=5              \
                              label.padding_left=5               \
                              icon.drawing=off                   \
                              script="$PLUGIN_DIR/spaces.sh"     \
                              click_script="yabai -m space --focus $sid"
done
