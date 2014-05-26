# file: ~/.xinitrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=2:sw=2:ft=sh:

#!/bin/sh

# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# turn of that 1981 bell :/
xset -b

# add archlinux font path, then rehash fonts
xset +fp /usr/share/fonts/local/
xset fp rehash

# hsetroot -fill ~/wallpaper/rocks.jpg
# hsetroot -fill ~/wallpaper/arviwm.png
hsetroot -fill ~/wallpaper/bridge.jpg

# use x keybindings as defined in ~/.xbindkeysrc
xbindkeys

# source .xresources
xrdb ~/.xresources

# start dwm_statusbar
dwm_statusbar &

exec dwm
