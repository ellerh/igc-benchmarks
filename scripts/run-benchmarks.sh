#!/usr/bin/env bash

print_usage() {
    printf "Usage: %s [-h] [-b BENCHMARK] [-i ITERATIONS] EMACS\n" "$0"
}

help() {
    print_usage
    names=$(for b in ${DEFAULT_BENCHMARKS[@]}; do
        printf '\t%s\n' $(basename -s .el "$b")
    done)
    printf 'Benchmarks:\n %s\n' "$names"
    exit 1
}

usage() {
    print_usage
    exit 1
}

set -eu

ITERATIONS=1
DEFAULT_BENCHMARKS=(
    scroll
    boehm-gc
    hash-equal
    hash-eql
    hash-eq
    compile
    print
    primes
    string
    taxicab
    pidigits
)
BENCHMARKS=()

while getopts b:i:h OPT; do
    case $OPT in
        b) BENCHMARKS+=($OPTARG) ;;
        i) ITERATIONS=$OPTARG ;;
        h) help ;;
        \?)
            echo "Invalid option: $OPTARG"
            usage
            ;;
    esac
done

if [ ${#BENCHMARKS[@]} = 0 ]; then
    for b in "${DEFAULT_BENCHMARKS[@]}"; do
        BENCHMARKS+=($b)
    done
fi

shift $(($OPTIND - 1))

case $# in
    1) EMACS="$1" ;;
    *) usage ;;
esac

CSVFILE=results.csv

for file in "../emacses/$EMACS"; do
    if [ ! -e $file ]; then
        printf "No such file: $file\n"
        exit 1
    fi
done

for BENCHMARK in "${BENCHMARKS[@]}"; do
    bash compile-benchmark.sh $EMACS $BENCHMARK || exit 2
done

ulimit -d $((512 * 1024))

trap '{
tmux -L gnutime kill-server
rm gnutime-fifo
echo "Interrupted  "
exit 3
}' INT

gnutime() {
    local output=$1 emacs=$2 benchmark=$3 args=$4
    if tmux -L gnutime has-session 2>/dev/null; then
        echo "existing tmux session (-L gnutime)"
        tmux -L gnutime list-sessions
        exit 3
    fi
    mkfifo gnutime-fifo
    local cmd="/usr/bin/time \
    --output '$output' --append \
    --format '$emacs,$benchmark,%e,%U,%S,%M' $args \
    ; echo \$? >gnutime-fifo"
    tmux -L gnutime new-session -d "$cmd"
    read -d '' ERR <gnutime-fifo
    rm gnutime-fifo
    return "$ERR"
}

if [ -e "$CSVFILE" ]; then
    mv --backup "$CSVFILE" "$CSVFILE".old
fi

printf "emacs,benchmark,elapsed,user,sys,rss-max\n" >"$CSVFILE"

for BENCHMARK in "${BENCHMARKS[@]}"; do
    for I in $(seq $ITERATIONS); do
        printf "$EMACS $BENCHMARK $I/$ITERATIONS ..." >&2
        gnutime "$CSVFILE" "$EMACS" "$BENCHMARK" \
            "../emacses/$EMACS -Q -nw \
            -l ../elisp/${BENCHMARK}.elc \
            -f main" || {
            printf "Emacs failed $?\n" >&2
            exit 3
        }
        printf "\r" >&2
        tail -n1 "$CSVFILE" >&2
    done
done
