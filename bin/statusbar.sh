#!/bin/sh

mute=$(amixer get Master | grep % --max-count 1 | sed "s/.*\[\([a-z]*\)\]/\1/")
if [ $mute == 'on' ]; then
  mute=''
else #'off'
  mute=''
fi

volume=$(amixer get Master | grep % --max-count 1 | sed "s/^.*\[\([0-9]*%\).*/\1/")

device=/dev/sda2
disk=$(df $device | grep $device | sed "s/.* \([0-9]*%\).*/\1/")

network=$(nmcli connection show --active | sed "1d;s/  .*$//")
if [ ! $network ]; then
  network="nc" #indicate if there is no connection
fi

battery_level=0
batteries=0
full_threshold=98
for battery in $(find /sys/class/power_supply/ -name "BAT*") ; do
  capacity=$(cat $battery/capacity)
  battery_level=$(expr $battery_level + $capacity)
  batteries=$(expr $batteries + 1)
done
battery_level=$(expr $battery_level / 2)
if [ $battery_level -ge $full_threshold ]; then
  battery_level=100
fi

date=$(date '+%Y-%m-%d %-I:%M %p')

sep="/"
echo " $mute $volume $sep  $disk $sep  $network $sep  $battery_level% $sep $date "

