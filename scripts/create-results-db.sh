#!/usr/bin/env bash

# Create a file results.sqlite with the an emtpy table for results.

set -eu

if [ $# != 0 ]; then
    echo "Usage: $0"
    exit 1
fi

FILENAME=results.sqlite

if [ -e $FILENAME ]; then
    printf "File exists: %s\n" "$FILENAME"
    exit 1
fi

printf "Initializing $FILENAME ..."
sqlite3 -cmd ".read schema.sql" -cmd ".save '$FILENAME'" </dev/null
echo " done."
