#!/bin/sh
# Snippet of code to set-up mutliple controllers
sudo xboxdrv --daemon --dbus system --id 0 --led 2 --detach-kernel-driver --next-controller --id 1 --led 3 --detach-kernel-driver
# Snippet of code to set-up an individual controller
#sudo xboxdrv --detach-kernel-driver
