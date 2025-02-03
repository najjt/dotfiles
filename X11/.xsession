# Set keyboard layouts and layout switching shortcut
setxkbmap -layout us,se -option grp:rshift_toggle

# Set keyboard repeat delay
xset r rate 300

# Hotkey daemon
sxhkd &

# Clipboard manager
greenclip daemon &

# Hide mouse cursor when not in use
xbanish -t 5 -s &

# Start the desktop environment
exec startxfce4
