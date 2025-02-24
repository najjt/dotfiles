# Set keyboard layouts and layout switching shortcut
setxkbmap -layout us,se -option grp:rshift_toggle

# Set keyboard repeat rate
xset r rate 300

# Hotkey daemon
sxhkd &

# Clipboard manager
greenclip daemon &

# Notifications
dunst &

# Hide mouse cursor when not in use
xbanish -t 5 -s &

# Start compositor
picom -b &

# Start the desktop environment
exec dbus-run-session startplasma-x11
