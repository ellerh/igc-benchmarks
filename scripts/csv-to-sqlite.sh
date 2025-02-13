#!/usr/bin/bash

set -eu

if [ -e "$2" ] ; then
    printf "File exists: %s\n" "$2"
    exit 1
fi

sqlite3 -cmd ".read schema.sql" \
	-cmd ".import --csv '$1' --skip 1 results" \
	-cmd ".save '$2'"  </dev/null
