#!/bin/sh

# Statusbar loop
while true; do
  xsetroot -name "$(statusbar.sh)"
  sleep 1
done &
