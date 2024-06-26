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

# From https://wiki.archlinux.org/title/HiDPI#X_Resources
if test -n "$DPI_CONFIG"; then
    xrdb -merge <(echo "Xft.dpi: $DPI_CONFIG")
fi

# Remap caps to esc
setxkbmap -option caps:escape

# Disable Palm rejection
xinput set-prop 'PIXA3854:00 093A:0274 Touchpad' 321 0

# Enable pixel-perfect scrolling in firefox
export MOZ_USE_XINPUT2=1
export XDG_SESSION_TYPE=x11
export GDK_BACKEND=x11
for var in DISPLAY XDG_SESSION_TYPE GDK_BACKEND; do
    dbus-update-activation-environment --systemd "$var"
done

if test -r "$HOME/.xinitrc.local"; then
    source "$HOME/.xinitrc.local"
fi
