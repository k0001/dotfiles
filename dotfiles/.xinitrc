#!/bin/sh

# I execute this script from `xdm`. I guess `startx` should work too.

# Arch users: Notice I'm using this xorg-xdm version, which adds
# ConsoleKit support https://bugs.archlinux.org/task/22671


# Common X stuff
xrdb -merge .Xresources
xsetroot -cursor_name left_ptr
#setxkbmap -layout es
#setxkbmap -model pc104 -layout us -variant intl
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl

# Disable touchpad while typing
syndaemon -t -k -i 2 -d

# Music Player Daemon
mpd ~/.mpd/mpd.conf

pidof mpdscribble >& /dev/null
if [ $? -ne 0 ]; then
      mpdscribble &
fi

# Sync clipboards using autocutsel
autocutsel -fork &
autocutsel -selection CLIPBOARD -fork &
autocutsel -selection PRIMARY -fork &

# IBus (see ~/.profile)
#ibus-daemon --xim -d

# Wallpaper
#xloadimage -onroot -fullscreen $HOME/keep-calm-and-curry-on.jpg

# Gnome Keyring Daemon
/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh

# XMonad
$HOME/.xmonad/xmonad-x86_64-linux


