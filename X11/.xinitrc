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
    greenclip clear && greenclip daemon &
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
which xbanish && if pgrep -x xbanish ; then
    echo xbanish already running
else
    xbanish -t 5 -s &
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
