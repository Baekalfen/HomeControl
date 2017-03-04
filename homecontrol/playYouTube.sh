#!/bin/bash
VURL=$(youtube-dl -g $1)
echo $VURL
HOUR=$(( ( RANDOM % 7 )  + 1 ))
MINUTE=$(( ( RANDOM % 59 )  + 0 ))
omxplayer "$VURL" --pos $HOUR:$MINUTE:00
