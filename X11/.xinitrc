# Set keyboard layouts and layout switching shortcut
setxkbmap -layout us,se -option grp:rshift_toggle

# Set keyboard repeat delay and rate
xset r rate 300 25

# Hotkey daemon
sxhkd &

# Notifications
dunst &

# Hide mouse cursor when not in use
xbanish -t 5 -s &

# Start the desktop environment
exec dbus-run-session startplasma-x11
