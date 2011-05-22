#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

xrdb -merge .Xresources
xsetroot -cursor_name left_ptr
setxkbmap -layout es
mpd ~/.mpd/mpd.conf
syndaemon -t -k -i 2 -d
exec $HOME/.xmonad/xmonad-x86_64-linux

