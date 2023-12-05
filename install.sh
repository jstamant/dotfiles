#!/usr/bin/bash

PACKAGES=$(find * -mindepth 0 -maxdepth 0 -type d -regex "^[^.].*")

for package in $PACKAGES
do
    echo "Re-stowing $package"
    stow --restow $package
done
