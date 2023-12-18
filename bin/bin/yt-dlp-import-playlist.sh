#!/bin/bash

echo "Please provide a link to a Youtube playlist"
read -p "Link: " playlist
yt-dlp -i -x --audio-format mp3 -o '%(playlist_index)s-%(title)s.%(ext)s' $playlist
