# Set keyboard layout and layout switching key
setxkbmap -layout us,se -option grp:rshift_toggle

# Set keyboard repeat delay
xset r rate 200

# Start sxhkd if it isn't running already
pgrep sxhkd || sxhkd &

# Start network manager applet if it isn't running already
pgrep nm-applet || nm-applet &

# Start bluetooth applet if it isn't running already
pgrep blueman-applet || blueman-applet &

# Start clipboard manager
greenclip daemon &

# Start the Xfce desktop environment
exec startxfce4
