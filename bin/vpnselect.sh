#!/bin/sh

configs=$(mktemp)
#for config in /usr/share/openvpn/nordvpn/*.ovpn; do
  ## This works, but takes a LONG time
  #basename "$config" ".ovpn" >> "$configs"
#done
#find /usr/share/openvpn/nordvpn -exec echo {} +
countries=$(grep -o "^[a-x][a-x]" "$configs")
country=$(cat "$countries" | dmenu -i)
selection=$(cat "$configs" | dmenu -i)
