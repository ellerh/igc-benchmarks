#!/usr/bin/bash

set -eu

declare -a EMACSES=(master no-mps igc)
declare -a ELFILES=(scroll boehm-gc hash-equal hash-eql hash-eq compile print)
REPEAT=3
CSVFILE=results.csv

ulimit -d $((1500 * 1024))

source cleanup.sh
source screen-pty.sh

for ELFILE in "${ELFILES[@]}" ; do
    "../emacses/${EMACSES[0]}" -Q -batch \
    -eval "(byte-compile-file \"../elisp/$ELFILE.el\")" \
    	|| exit 2
done

TIMEPID=''
function kill_time_children {
    if [ -n "$TIMEPID" ] ; then
	for CHILD in $(ps --no-headers --ppid "$TIMEPID" -o '%p') ; do
	    kill -SIGHUP $CHILD || :
	done
    fi
}

CLEANUP_ACTIONS+=(kill_time_children)

if [ -e "$CSVFILE" ]; then
    mv --backup=numbered "$CSVFILE" "$CSVFILE".old
fi

printf "emacs,benchmark,elapsed,user,sys,rss-max\n" >"$CSVFILE"
for EMACS in "${EMACSES[@]}" ; do
    for ELFILE in "${ELFILES[@]}" ; do
	for I in $(seq $REPEAT) ; do
	    printf "$EMACS $ELFILE $I ..." >&2
	    /usr/bin/time \
		--output "$CSVFILE" --append \
		--format "$EMACS,$ELFILE,%e,%U,%S,%M" \
		"../emacses/$EMACS" -Q -nw -t "$SCREENPTY" \
		-l "../elisp/${ELFILE}.elc" \
		-f main 2>/dev/null &
	    TIMEPID=$!
	    wait $! || { echo "Emacs failed" >&2 ; exit 3 ; }
	    printf "\r" >&2
	    tail -n1 "$CSVFILE" >&2
	done
    done
done
