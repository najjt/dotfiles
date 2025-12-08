if [[ -o login && -t 0 ]]; then
    source $ZDOTDIR/.zshrc
    emacs --daemon > /dev/null 2>&1 &

    # Start X only if DISPLAY is not set (i.e. not already in X)
    if [[ -z "$DISPLAY" ]]; then
        echo "Starting i3wm in 3 seconds..."
        sleep 3
        echo "\nStarting i3wm now"
        exec /usr/bin/startx $(echo $XINITRC) i3 -- -ardelay 300 -arinterval 25
    fi
fi
