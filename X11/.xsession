# Set keyboard layout and layout switching key
setxkbmap -layout us,se -option grp:ctrl_alt_toggle

# Set keyboard repeat delay
xset r rate 200

# Start sxhkd if it isn't running already
pgrep sxhkd || sxhkd &

# Shuffle desktop wallpaper
sh $HOME/scripts/shuffle_wallpaper.sh

# Start the Xfce desktop environment
exec startxfce4
