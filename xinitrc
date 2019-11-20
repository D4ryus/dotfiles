#!/bin/bash
# -*- mode: shell-script; indent-tabs-mode: nil; sh-basic-offset: 4; -*-
# file: ~/.xinitrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=4:sw=4:ft=sh:

if test -d "/etc/X11/xinit/xinitrc.d"; then
    for f in "/etc/X11/xinit/xinitrc.d/*"; do
        test -x "$f" && . "$f"
    done
    unset f
fi

# turn of bell
xset -b

# change keyboard repeat rate to 50, delay to 200
xset r rate 200 50

# add archlinux font path, then rehash fonts
if test -d "/usr/share/fonts/local/"; then
    xset +fp "/usr/share/fonts/local/"
    xset fp rehash
fi

if test -r "$HOME/.xresources"; then
    xrdb -merge "$HOME/.xresources"
fi

if test -r "$HOME/.xresources.local"; then
    xrdb -merge "$HOME/.xresources.local"
fi

if test -r "$HOME/.xinitrc.local"; then
    source "$HOME/.xinitrc.local"
fi
