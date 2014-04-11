#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

while true
do
    IP=$(for i in `ip r`; do echo $i; done | grep -A 1 src | tail -n1) # can get confused if you use vmware
    LOCALTIME=$(date +%Y-%m-%d\ %H:%M)
    TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"
    RAM=$(free -m | awk '/cache:/ { print $3"M" }')
    VOL=$(amixer get Master | tail -1 | awk '{print $6 " " $4}' | sed -e 's/\[//g' -e 's/\]//g')
    if acpi -a | grep off-line > /dev/null
    then
        BAT="$(acpi -b | awk '{ print $4 }' | tr -d ',')"
        xsetroot -name "[ip: $IP] [bat: $BAT] [vol: $VOL] [temp: $TEMP] [ram: $RAM] [$LOCALTIME]"
    else
        xsetroot -name "[ip: $IP] [vol: $VOL] [temp: $TEMP] [ram: $RAM] [$LOCALTIME]"
    fi
    sleep 10s
done &

# turn of that 1981 bell :/
xset -b

# add archlinux font path, then rehash fonts
xset +fp /usr/share/fonts/local/
xset fp rehash

# use x keybindings as defined in ~/.xbindkeysrc
xbindkeys

exec /home/d4ryus/software/dwm/usr/local/bin/dwm
