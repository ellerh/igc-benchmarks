#!/usr/bin/bash

SCREENPIPE=$(mktemp -u)
mkfifo "$SCREENPIPE"

function screen_cleanup {
    screen -r "$SCREENPID" -X quit
    rm "$SCREENPIPE"
}

CLEANUP_ACTIONS+=(screen_cleanup)

screen -i -S Benchmark -D -m &
SCREENPID=$!
until screen -r "$SCREENPID" -X version >/dev/null; do
    sleep 0.05
done
screen -r "$SCREENPID" -X stuff "tty > '$SCREENPIPE' && sleep 10000 ; \r"
SCREENPTY=$(cat "$SCREENPIPE")
