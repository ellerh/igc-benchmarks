#!/usr/bin/bash

set -eu

function usage {
    local PROG=$(basename $0)
    printf "Usage: %s CMD ARGS...

where CMD can be one of:

  full <db>		show the full database
  avg <db>         	show averages
  b/e <db> <col>   	show benchmark by emacs for column <col>
  plot <db> <col>      	show a bar diagram for column <col>
  rel <db> <base>       show averages relative to base
  b/e-rel <db> <base> <col>   	same as b/e but for relative values 
  plot-rel <db> <base> <col>  	same as plot but for realtive values

<col> can be one of real,user,sys,rss_max,traced_time,ncollections.

" "$PROG"
}

function full {
    local DB=$1
    sqlite3 -cmd ".mode column" \
	    -cmd "select * from results order by benchmark,emacs
" "$DB" </dev/null 
}

function avg {
    local DB=$1
    sqlite3 -cmd ".mode column" \
	    -cmd "select * from results_avg_round order by benchmark,emacs" \
	    "$DB" </dev/null 
}

function rel {
    local DB=$1 BASE=$2
    sqlite3 -cmd ".mode column" \
	    -cmd " select * from results_rel_round
	    	    where base='$BASE'
		    order by benchmark,emacs
" "$DB" </dev/null 
}

function benchmark_by_emacs {
    local DB=$1 TABLE=$2 COL=$3 
    sqlite3 -cmd ".mode column --wrap 80" \
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
	real|user|sys|rss_max|traced_time|ncollections);;
	*)
	    printf "Invalid column: %s\n" "$COL"
	    exit 2;;
    esac
}

function check_base {
    local DB=$1 BASE=$2
    if [ $(sqlite3 "$DB" \
	   "select ('$BASE' in (select distinct emacs from results))") \
	 != "1" ] ; then
       printf "Invalid base: %s\n" "$BASE" 
       exit 3
    fi
}

function main {
    case $# in
	2|3|4)
	    case "$# $1" in
		"2 full") full "$2";;
		"2 avg") avg "$2";;
		"3 rel")
		    check_base "$2" "$3"
		    rel "$2" "$3";;
		"3 b/e")
		    check_column "$3"
		    benchmark_by_emacs "$2" results "$3";;
		"3 plot")
		    check_column "$3"
		    python3 bardiagram.py "$2" results "$3";;
		"4 b/e-rel")
		    check_column "$4"
		    check_base "$2" "$3"
		    benchmark_by_emacs "$2" \
				       "(select * from results_rel_round
				          where base='$3')" \
				       "$4";;
		"4 plot-rel")
		    check_column "$4"
		    check_base "$2" "$3"
		    python3 bardiagram.py "$2" \
			    "(select * from results_rel where base='$3')" \
			    "$4";;
		"4 plot-rel-diff")
		    check_column "$4"
		    check_base "$2" "$3"
		    python3 bardiagram.py "$2" \
			    "(select * from results_rel_diff
			       where base='$3')" \
			    "$4";;
		*) usage;;
	    esac;;
	*) usage;;
    esac
}

main "$@"
