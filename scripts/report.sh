#!/usr/bin/env bash

# Show various resports results table in the DB results.sqlite

set -eu

DB=results.sqlite

function usage {
    local PROG=$(basename $0)
    printf "Usage: %s CMD ARGS...

where CMD can be one of:

  full 		show the full database
  avg          	show averages
  b/e <col>   	show benchmark by emacs for column <col>
  plot <col>    show a bar diagram for column <col>
  rel <base>    show averages relative to base
  b/e-rel <base> <col>   	same as b/e but for relative values 
  plot-rel <base> <col>  	same as plot but for realtive values

<col> can be one of real,user,sys,rss_max

" "$PROG"
}

function full {
    sqlite3 -cmd ".mode column" \
        -cmd "select * from results order by benchmark,emacs
" "$DB" </dev/null
}

function avg {
    sqlite3 -cmd ".mode column" \
        -cmd "select * from results_avg_round order by benchmark,emacs" \
        "$DB" </dev/null
}

function rel {
    local BASE=$1
    sqlite3 -cmd ".mode column" \
        -cmd " select * from results_rel_round
	    	    where base='$BASE'
		    order by benchmark,emacs
" "$DB" </dev/null
}

function benchmark_by_emacs {
    local TABLE=$1 COL=$2
    sqlite3 -cmd ".mode column --wrap 0" \
        -cmd ".header off " \
        -cmd "
with strings as (select emacs,benchmark,format('%.2f', $COL) as r
     		    from $TABLE group by emacs,benchmark),
     lens as (select benchmark,max(max(length(r)),length(benchmark)) len
                from strings group by benchmark)
select '',string_agg(format('%*s',len,benchmark),' ' order by benchmark)
  from strings natural join lens
 group by emacs
union 
select emacs,string_agg(format('%*s', len, r), ' ' order by benchmark)
  from strings natural join lens
 group by emacs
 order by emacs;
" "$DB" </dev/null
}

function check_column {
    local COL=$1
    case "$COL" in
        real | user | sys | rss_max) ;;
        *)
            printf "Invalid column: %s\n" "$COL"
            exit 2
            ;;
    esac
}

function check_base {
    local BASE=$1
    if
        [ $(sqlite3 -cmd ".headers off" \
            "$DB" \
            "select ('$BASE' in (select distinct emacs from results))") \
            = "1" ]
    then
        return 0
    fi
    printf "Invalid base: %s\n" "$BASE"
    exit 3
}

function main {
    case $# in
        1 | 2 | 3)
            case "$# $1" in
                "1 full") full ;;
                "1 avg") avg ;;
                "2 rel")
                    check_base "$2"
                    rel "$2"
                    ;;
                "2 b/e")
                    check_column "$2"
                    benchmark_by_emacs results "$2"
                    ;;
                "2 plot")
                    check_column "$2"
                    python3 bardiagram.py "$DB" results "$2"
                    ;;
                "3 b/e-rel")
                    check_column "$3"
                    check_base "$2"
                    benchmark_by_emacs \
                        "(select * from results_rel_round
				          where base='$2')" \
                        "$3"
                    ;;
                "3 plot-rel")
                    check_column "$3"
                    check_base "$2"
                    python3 bardiagram.py "$DB" \
                        "(select * from results_rel where base='$2')" \
                        "$3"
                    ;;
                "3 plot-rel-diff")
                    check_column "$3"
                    check_base "$2"
                    python3 bardiagram.py "$DB" \
                        "(select * from results_rel_diff
			       where base='$2')" \
                        "$3"
                    ;;
                *) usage ;;
            esac
            ;;
        *) usage ;;
    esac
}

main "$@"
