!/bin/sh

# I execute this script from `xdm`. I guess `startx` should work too.

# Arch users: Notice I'm using this xorg-xdm version, which adds
# ConsoleKit support https://bugs.archlinux.org/task/22671


# Common X stuff
xrdb -merge .Xresources
xsetroot -cursor_name left_ptr
setxkbmap -layout es

# Disable touchpad while typing
syndaemon -t -k -i 2 -d

# Music Player Daemon
mpd ~/.mpd/mpd.conf

# Skype shit.
$HOME/bin/skype-guiless \
    skyped -c $HOME/.skyped/skyped.conf \
           --log $HOME/.skyped/skyped.log \
           --host 127.0.0.1 \
           --nofork &

# XMonad
$HOME/.xmonad/xmonad-x86_64-linux

