#!/bin/sh

# A simple UI for detecting and selecting displays.
# Detects connected displays using 'xrandr' and prompts user to select one using
# 'dmenu'.

# User may also select "manual selection" which opens arandr. NOT IMPLEMENTED
# User may also select "multi-monitor" to mirror displays, or go 2-3 external
# displays. NOT IMPLEMENTED PROPERLY.

# Credit to Luke Smith's idea and initial script, found on his github:
# https://github.com/LukeSmithxyz/voidrice/blob/master/.scripts/i3cmds/displayselect

# Handler for configuring multi-monitor set-up
multimon() {
  case "$(echo "$displays" | wc -l)" in
    1) xrandr $(echo "$allports" | grep -v "$displays" | awk '{print "--output", $1, "--off"}' | tr '\n' ' ') ;;
    2) twoscreen ;;  # See function below
    *) morescreen ;; # See function below
  esac
}

# If multi-monitor is desired, and there are exactly two displays connected:
twoscreen() {
  mirror=$(printf "no\\nyes" | dmenu -i -p "Mirror displays?")
  # Mirror displays using native resolution of external display and a scaled
  # version for the internal display
  if [ "$mirror" = "yes" ]; then
    external=$(echo "$displays" | dmenu -i -p "Optimize resolution for:")
    internal=$(echo "$displays" | grep -v "$external")

    res_external=$(xrandr | sed -n "/^$external/,/\+/p" | \
      tail -n 1 | awk '{print $1}')
    res_internal=$(xrandr | sed -n "/^$internal/,/\+/p" | \
      tail -n 1 | awk '{print $1}')

    res_ext_x=$(echo "$res_external" | sed 's/x.*//')
    res_ext_y=$(echo "$res_external" | sed 's/.*x//')
    res_int_x=$(echo "$res_internal" | sed 's/x.*//')
    res_int_y=$(echo "$res_internal" | sed 's/.*x//')

    scale_x=$(echo "$res_ext_x / $res_int_x" | bc -l)
    scale_y=$(echo "$res_ext_y / $res_int_y" | bc -l)

    xrandr --output "$external" --auto --scale 1.0x1.0 \
      --output "$internal" --auto --same-as "$external" \
      --scale "$scale_x"x"$scale_y"
  else # "no", mirror displays not desired:
    primary=$(echo "$displays" | dmenu -i -p "Select primary display:")
    secondary=$(echo "$displays" | grep -v "$primary")
    direction=$(printf "left\\nright" | dmenu -i -p "What side of $primary should $secondary be on?")
    xrandr --output "$primary" --auto --output "$secondary" --"$direction"-of "$primary" --auto
  fi
}

# If multi-monitor is desired, and there are more than two displays connected:
morescreen() {
  primary=$(echo "$displays" | dmenu -i -p "Select primary (center) display:")
  secondary=$(echo "$displays" | grep -v "$primary" | dmenu -i -p "Select secondary display:")
  direction=$(printf "left\\nright" | dmenu -i -p "What side of $primary should $secondary be on?")
  tertiary=$(echo "$displays" | grep -v "$primary" | grep -v "$secondary" | dmenu -i -p "Select third display:")
  xrandr --output "$primary" --auto \
    --output "$secondary" --"$direction"-of "$primary" --auto \
    --output "$tertiary" --"$(printf "left\\nright" | grep -v "$direction")"-of "$primary" --auto
}

# Get all possible display ports
allports=$(xrandr | grep "connected")

# Get all connected displays
displays=$(echo "$allports" | grep " connected" | awk '{print $1}')

# Get user choice including multi-monitor and manual selection:
chosen=$(printf "%s\\nmulti-monitor\\nmanual selection" "$displays" | dmenu -i -p "Select display arangement:")
notchosen=$(echo "$allports" | grep -v "^$chosen" | awk '{print $1}')

if [ "$chosen" ]; then
  case "$chosen" in
    "manual selection") arandr ; exit ;;
    "multi-monitor") multimon ;;
    *) xrandr --output "$chosen" --auto;
      for output in $notchosen; do
        xrandr --output "$output" --off
      done
  esac
  resetwallpaper.sh
fi
