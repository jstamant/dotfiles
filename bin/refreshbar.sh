#!/bin/sh
# Refresh the statusbar by skipping the sleep command in autostart.sh
process=$(pstree -lp | grep "autostart\.sh.*sleep" | sed "s/.*sleep(\([0-9]*\)).*/\1/")
if [ -n "$process" ]; then
  kill "$process"
fi
