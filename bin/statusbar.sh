#!/bin/sh

# CPU usage not too accurate: it only adds the usage of the 3 top CPU processes
#cpu=$(ps -eo "%C" --sort=-pcpu | sed -n '2,4p' | \
#  awk '{ cpu+=$1 } END { print cpu }' | sed 's/\(^.*$\)/\1%/')

#memory=$(free -h | sed -n 2p | awk '{ print $3 }' )

mute=$(amixer get Master | grep --max-count=1 -o "\[[a-z]*\]")
volume=$(amixer get Master | grep --max-count=1 -o "[0-9]*%" | grep -o "[0-9]*")
if [ "$mute" = '[on]' ]; then
  case "$volume" in
    7[6-9]|8[0-9]|9[0-9]|100) mute="" ;;
    5[0-9]|6[0-9]|7[0-5])     mute="" ;;
    2[6-9]|3[0-9]|4[0-9])     mute="" ;;
    *)                        mute="" ;;
  esac
else #'[off]'
  mute=""
fi

device=/dev/sda2
disk=$(df $device | grep $device | sed "s/.* \([0-9]*%\).*/\1/")

network=$(nmcli connection show --active | sed "1d;s/  .*$//")
if [ ! "$network" ]; then
  network="nc" #indicate if there is no connection
fi

# This script doesn't take into account different battery capacities (mAh)
# It assumes both batteries have the same capacity
battery_level=0
battery_label=""
batteries=0
full_threshold=98
for battery in /sys/class/power_supply/BAT*; do
  capacity=$(cat "$battery"/capacity)
  battery_level=$((battery_level + capacity))
  batteries=$((batteries + 1))
done
battery_level=$((battery_level / 2))
if [ "$battery_level" -ge $full_threshold ]; then
  battery_level=100
fi
case "$battery_level" in
  8[0-9]|9[0-9]|100) battery_label="" ;;
  6[0-9]|7[0-9])     battery_label="" ;;
  4[0-9]|5[0-9])     battery_label="" ;;
  2[0-9]|3[0-9])     battery_label="" ;;
  *)                 battery_label="" ;;
esac
charging=$(cat /sys/class/power_supply/AC/online)
if [ "$charging" = "1" ]; then
  battery_label=""
fi

date=$(date '+%Y-%m-%d %-I:%M %p')

sep="/"
echo " $mute $volume% $sep  $disk $sep  $network $sep $battery_label $battery_level% $sep $date "
#echo "  $cpu $sep  $memory $sep $mute $volume $sep  $disk $sep  $network $sep  $battery_level% $sep $date "
