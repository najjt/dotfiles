# Set keyboard layouts and layout switching shortcut
setxkbmap -layout us,se -option grp:rshift_toggle

# Set keyboard repeat delay and rate
xset r rate 300 25

# Disable monitor power saving
xset -dpms

# Hotkey daemon
sxhkd &

# Clipboard manager
greenclip daemon &

# Notifications
dunst &

# Hide mouse cursor when not in use
unclutter &

# Choose session,
# KDE Plasma kept as default
session=${1:-kde}

case $session in
    i3                ) exec /usr/bin/i3;;
    kde               ) exec /usr/bin/startplasma-x11;;
    # No known session, try to run it as command
    *                 ) exec $1;;
esac
