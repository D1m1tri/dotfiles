#!/bin/bash

title=$(playerctl metadata title)
artist=$(playerctl metadata artist)
volume=$(pamixer --get-volume-human)

echo "$title | $artist - $volume"
