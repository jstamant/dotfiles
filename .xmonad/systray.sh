#!/bin/sh
sleep 60 && killall trayer
trayer --edge top\
       --align right\
       --SetDockType true\
       --SetPartialStrut true\
       --expand true\
       --height 17\
       --transparent true\
       --alpha 0\
       --tint 0x2d2d2d\
       --widthtype request\
       --monitor "primary"\
       --margin 0
