# Set keyboard layouts and layout switching shortcut
setxkbmap -layout us,se -option grp:rshift_toggle

# Disable monitor power saving
xset -dpms

# Disable screen saver
xset s off

# Hotkey daemon
which sxhkd && if pgrep -x sxhkd ; then
    echo sxhkd already running
else
    sxhkd &
    disown
fi

# Clipboard manager
which greenclip && if pgrep -x greenclip ; then
    echo greenclip already running
else
    greenclip daemon &
    disown
fi

# Notifications
which dunst && if pgrep -x dunst ; then
    echo dunst already running
else
    dunst &
    disown
fi

# Hide mouse cursor when not in use
which unclutter && if pgrep -x unclutter ; then
    echo unclutter already running
else
    unclutter &
    disown
fi

# Choose session,
# KDE Plasma kept as default
session=${1:-kde}

case $session in
    i3                ) exec /usr/bin/i3;;
    kde               ) exec /usr/bin/startplasma-x11;;
    # No known session, try to run it as command
    *                 ) exec $1;;
esac
