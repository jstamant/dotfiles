#!/bin/bash

# This script retroactively commits the currently staged files
echo "Please provide a date in the form 'Mar 16' or 'Mar 16 12:00'."
read -p "Date: " commitdate
echo "Please provide a commit message."
read -p "Message: " commitmessage
echo

echo "Proceed with the following command?"
echo "git commit --date=\"\$(date --date='$commitdate' -R)\" -m \"$commitmessage\""
read -p "[y/N]: " -n 1 -r
echo
if [[ $REPLY =~ ^[yY]$ ]]
then
    git commit --date="$(date --date="$commitdate" -R)" -m "$commitmessage"
fi
