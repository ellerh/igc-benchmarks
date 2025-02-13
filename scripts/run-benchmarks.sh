#!/usr/bin/bash

set -eu

declare -a EMACSES=(master igc)
declare -a ELFILES=(scroll boehm-gc hash-equal hash-eql hash-eq compile print)
REPEAT=1
CSVFILE=results.csv
TRACETOOL=tracetool_stap

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

function tracetool_gnutime {
    local output=$1 emacs=$2 benchmark=$3
    shift 3
    /usr/bin/time --output "$output" --append \
		  --format "$emacs,$benchmark,%e,%U,%S,%M,0,0,0,0,0" \
		  "$@"
}

function tracetool_stap {
    local output=$1 emacs=$2 benchmark=$3
    local features=$(../emacses/$emacs \
			 -Q -batch \
			 -eval '(princ system-configuration-features)')
    local stpfile
    case "$features" in
	*\ MPS\ *)  stpfile=bench-mps.stp ;;
	*) stpfile=bench-master.stp ;;
    esac
    shift 3
    local tmpfile=$(mktemp)
    CLEANUP_ACTIONS+=("rm '$tmpfile'")
    stap -c "$*" -o "$tmpfile" $stpfile "$emacs" "$benchmark"
    cat "$tmpfile" >> "$output"
}

if [ -e "$CSVFILE" ]; then
    mv --backup=numbered "$CSVFILE" "$CSVFILE".old
fi

printf "emacs,benchmark,\
elapsed,user,sys,rss-max,\
gc-sum,gc-avg,gc-max,gc-min,gc-count\n" >"$CSVFILE"

for EMACS in "${EMACSES[@]}" ; do
    for ELFILE in "${ELFILES[@]}" ; do
	for I in $(seq $REPEAT) ; do
	    printf "$EMACS $ELFILE $I ..." >&2 
	    $TRACETOOL "$CSVFILE" "$EMACS" "$ELFILE" \
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
