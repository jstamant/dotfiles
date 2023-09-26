#!/bin/bash
# The script that's responsible for running swhkd.
# This gets triggered from hotkeys.service (a user-level systemd unit)
# Manipulate the unit with `systemctl --user <start/enable> hotkeys.service>

killall swhks
swhks &
pkexec swhkd --config /home/jstamant/.config/swhkd/swhkdrc

