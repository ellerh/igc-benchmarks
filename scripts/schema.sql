
create table results (
  emacs text not null,
  benchmark text not null,
  real real not null,
  user real not null,
  sys real not null,
  rss_max integer not null,
  gc_sum integer not null,
  gc_avg integer not null,
  gc_max integer not null,
  gc_min integer not null,
  gc_count integer not null
) strict;

create view results_avg as
  select benchmark, emacs,
	 avg(real) real,
	 avg(user) user,
	 avg(sys) sys,
	 avg(rss_max) rss_max,
	 avg(gc_sum) gc_sum,
	 avg(gc_avg) gc_avg,
	 avg(gc_max) gc_max,
	 avg(gc_min) gc_min,
	 avg(gc_count) gc_count
    from results
   group by benchmark, emacs;

create view results_rel as
  select benchmark,
	 r.emacs emacs,
	 base.emacs base,
       	 r.real/base.real real,
	 r.user/base.user user,
	 r.sys/base.sys sys,
 	 r.rss_max/base.rss_max rss_max,
	 r.gc_sum/base.gc_sum gc_sum,
	 r.gc_avg/base.gc_avg gc_avg,
	 r.gc_max/base.gc_max gc_max,
	 r.gc_min/base.gc_min gc_min,
	 r.gc_count/base.gc_count gc_count
    from results_avg r join results_avg base using (benchmark);

create view results_rel_round as
  select benchmark, emacs, base,
	 round(real, 2) real,
	 round(user, 2) user,
	 round(sys, 2) sys,
	 round(rss_max, 2) rss_max
    from results_rel;

