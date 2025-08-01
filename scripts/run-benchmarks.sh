#!/usr/bin/bash

set -eu

declare -a EMACSES=(
    igc
    wgc
    master
    master-0.75
    master-1.0
    master-1.5
    #igc headerless
    #master
    #no-mps no-mps-headerless
)

declare -a ELFILES=(
    scroll
    boehm-gc
    hash-equal
    hash-eql
    hash-eq
    compile
    print
    primes
    #primesv
    #sumloop
    string
    taxicab
    pidigits
)

REPEAT=1
CSVFILE=results.csv
#TRACETOOL=tracetool_stap
TRACETOOL=tracetool_gnutime

ulimit -d $((512 * 1024))

source cleanup.sh

for ELFILE in "${ELFILES[@]}"; do
    "../emacses/${EMACSES[0]}" -Q -batch \
        -eval "(byte-compile-file \"../elisp/$ELFILE.el\")" ||
        exit 2
done

function quit_screen {
    local screenpid="$1"
    screen -r "$screenpid" -X quit
}

function tracetool_gnutime {
    local output=$1 emacs=$2 benchmark=$3
    shift 3
    exec screen -D -m /usr/bin/time --output "$output" --append \
        --format "$emacs,$benchmark,%e,%U,%S,%M,0,0" \
        "$@"
}

function tracetool_stap {
    local output=$1 emacs=$2 benchmark=$3
    local features=$(../emacses/$emacs \
        -Q -batch \
        -eval '(princ system-configuration-features)')
    local stpfile
    case "$features" in
        *\ MPS\ *) stpfile=bench-mps.stp ;;
        *) stpfile=bench-master.stp ;;
    esac
    shift 3
    local tmpfile=$(mktemp)
    CLEANUP_ACTIONS+=("rm '$tmpfile'")
    stap -c "$*" -o "$tmpfile" $stpfile "$emacs" "$benchmark"
    cat "$tmpfile" >>"$output"
}

if [ -e "$CSVFILE" ]; then
    mv --backup=numbered "$CSVFILE" "$CSVFILE".old
fi

printf "emacs,benchmark,\
elapsed,user,sys,rss-max,\
traced-time,ncollections\n" >"$CSVFILE"

for EMACS in "${EMACSES[@]}"; do
    for ELFILE in "${ELFILES[@]}"; do
        for I in $(seq $REPEAT); do
            printf "$EMACS $ELFILE $I ..." >&2
            $TRACETOOL "$CSVFILE" "$EMACS" "$ELFILE" \
                "../emacses/$EMACS" -Q -nw \
                -l "../elisp/${ELFILE}.elc" \
                -f main &
            CLEANUP_ACTIONS+=("screen -r $! -X quit")
            SCREENPID=$!
            wait $! || {
                printf "Emacs failed %d" "$?" >&2
                exit 3
            }
            printf "\r" >&2
            unset CLEANUP_ACTIONS[-1]
            tail -n1 "$CSVFILE" >&2
        done
    done
done
