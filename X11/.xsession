# Set keyboard layout and layout switching key
setxkbmap -layout us,se -option grp:rshift_toggle

# Set keyboard repeat delay
xset r rate 200

# Start sxhkd
sxhkd &

# Start clipboard manager
greenclip daemon &

# Hide mouse cursor when not in use
xbanish -t 5 -s &

# Shuffle wallpaper
sh $HOME/scripts/shuffle_wallpaper.sh

# Start the Xfce desktop environment
exec startxfce4
