#!/usr/bin/env bash

if [ $# != 2 ] ; then
    printf "Usage: %s <emacs> <benchmark>\n" "$0"
    exit 1
fi

EMACS="$1"
BENCHMARK="$2"

exec "../emacses/$EMACS" -Q -batch \
        -eval "(byte-compile-file \"../elisp/$BENCHMARK.el\")"
