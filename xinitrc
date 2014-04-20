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

#while true
#do
#    IP=$(for i in `ip r`; do echo $i; done | grep -A 1 src | tail -n1) # can get confused if you use vmware
#    LOCALTIME=$(date +%Y-%m-%d\ %H:%M)
#    TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"
#    RAM=$(free -m | awk '/cache:/ { print $3"M" }')
#    VOL=$(amixer get Master | tail -1 | awk '{print $6 " " $4}' | sed -e 's/\[//g' -e 's/\]//g')
#    BAT="$(acpi -b | awk '{ print $3 " " $4 }' | tr -d ',')"
#    xsetroot -name "[ip: $IP] [bat: $BAT] [vol: $VOL] [temp: $TEMP] [ram: $RAM] [$LOCALTIME]"
#    sleep 5s
#done &

dwm_statusbar &

# turn of that 1981 bell :/
xset -b

# add archlinux font path, then rehash fonts
xset +fp /usr/share/fonts/local/
xset fp rehash

hsetroot -fill ~/wallpaper/rocks.jpg

# use x keybindings as defined in ~/.xbindkeysrc
xbindkeys

exec dwm
