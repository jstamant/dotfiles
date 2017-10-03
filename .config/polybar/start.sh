#!/bin/sh

# Terminate all running instances of polybar
killall -q polybar

# Wait for the process to terminate
while pgrep polybar > /dev/null; do sleep 1; done

# Launch polybar
polybar main &
