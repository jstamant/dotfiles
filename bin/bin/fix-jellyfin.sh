#!/usr/bin/env bash

sudo find /srv/jellyfin -type d -exec chmod 775 -- {} +
sudo find /srv/jellyfin -type f -exec chmod 664 -- {} +
sudo chown -R jellyfin:jellyfin /srv/jellyfin

