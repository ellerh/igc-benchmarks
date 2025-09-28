#!/usr/bin/env bash

# Import results from the CSV file results.csv to the sqlite DB
# results.sqlite.

set -eu

if [ $# != 0 ]; then
    echo "Usage: $0"
    exit 1
fi

FILENAME=results.csv
DB=results.sqlite

printf "Importing %s -> %s ..." "$FILENAME" "$DB"
sqlite3 -cmd ".import --csv '$FILENAME' --skip 1 results" \
    -cmd ".quit" \
    "$DB" </dev/null
echo " done."
