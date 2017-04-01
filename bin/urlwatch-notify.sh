#!/bin/sh
DATETIME=$(date +%F-%H-%M-%S)
FILE=$DATETIME-URLWATCH-NOTIFICATION.txt
urlwatch > ~/downloads/$FILE
find ~/downloads -size 0 -name $FILE -delete

