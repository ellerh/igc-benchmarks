
import sqlite3
import matplotlib.pyplot as plt
import itertools
import sys
import re

def main() -> int:
    (_, db, table, col) = sys.argv;
    con = sqlite3.connect(db)
    cur = con.cursor ()
    res = cur.execute(f"""
    select benchmark,emacs,avg({col}),min({col}),max({col}) from {table}
    group by benchmark,emacs
    order by emacs,benchmark
    """)
    tab = res.fetchall()
    d = [(emacs, list(group))
         for emacs,group in itertools.groupby(tab, lambda r: r[1]) ]
    fig, ax = plt.subplots()
    nemacses = len(d)
    barwidth = 1/(nemacses + 1)
    for (i,(emacs, group)) in enumerate(d):
        x = [i * barwidth + j for j in range(0,len(group))]
        width = [avg for (_,_,avg,_,_) in group]
        emin = [avg - min for (_,_,avg,min,_) in group]
        emax = [max - avg for (_,_,avg,_,max) in group]
        ax.barh(x, width, barwidth, label=emacs, xerr=[emin,emax])
    nbenchmarks = len(d[0][1])
    ax.set_yticks(list(range(0, nbenchmarks)),
                  [benchmark for (benchmark,_,_,_,_) in d[0][1]])
    ax.invert_yaxis()  # labels read top-to-bottom
    ax.legend(loc='lower right')
    ax.set_xlabel(f'{col} [{unit_from_column (table, col)}]')
    plt.show()
    
    return 0

def unit_from_column(table, col):
    if re.search(r"select", table):
        return "1"
    match col:
        case "real"|"user"|"sys":
            return "s"
        case "rss_max":
            return "KiB"
        case "gc_sum"|"gc_avg"|"gc_max"|"gc_max":
            return "us"
        case _:
            return "??"

if __name__ == '__main__':
    sys.exit(main()) 
