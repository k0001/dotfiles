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

pidof mpdscribble >& /dev/null
if [ $? -ne 0 ]; then
      mpdscribble &
fi

# Sync clipboards using autocutsel
autocutsel -selection CLIPBOARD -fork &
autocutsel -selection PRIMARY -fork &

# XMonad
$HOME/.xmonad/xmonad-x86_64-linux

