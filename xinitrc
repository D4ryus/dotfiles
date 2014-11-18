# file: ~/.xinitrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=2:sw=2:ft=sh:

#!/bin/zsh

# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
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
if [ -r ~/.xresources ]; then
        xrdb -merge ~/.xresources
fi

if [ -r ~/.xresources.local ]; then
        xrdb -merge ~/.xresources.local
fi

if [ -r ~/.xinitrc.local ]; then
        source ~/.xinitrc.local
fi
