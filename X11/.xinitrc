# Sleep after 3 min, suspend after 5 min
xset 180 300 &
# Set keyboard repeat rate
xset r rate 200


# Hotkey daemon
sxhkd &

# Clipboard manager
greenclip daemon &

# Hide mouse cursor when not in use
xbanish -t 5 -s &

# Shuffle wallpaper
sh $HOME/scripts/shuffle_wallpaper.sh

# Set screen color temperature
xsct 3500

# Compositor
picom -b &

# Start i3
exec i3
