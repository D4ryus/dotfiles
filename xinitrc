# -*- mode: shell-script; indent-tabs-mode: nil; sh-basic-offset: 4; -*-
# file: ~/.xinitrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=4:sw=4:ft=sh:

#!/bin/zsh

# Executed by startx (run your window manager from here)

if test -d /etc/X11/xinit/xinitrc.d ; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        test -x "$f" && . "$f"
    done
    unset f
fi

# turn of that 1981 bell :/
xset -b

# change keyboard repeat rate to 50, delay 200
xset r rate 200 50

# add archlinux font path, then rehash fonts
xset +fp /usr/share/fonts/local/
xset fp rehash

# source .xresources
if test -r ~/.xresources ; then
    xrdb -merge ~/.xresources
fi

if test -r ~/.xresources.local ; then
    xrdb -merge ~/.xresources.local
fi

if test -r ~/.xinitrc.local ; then
    source ~/.xinitrc.local
fi
