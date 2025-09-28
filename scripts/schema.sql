
create table results (
  emacs text not null,
  benchmark text not null,
  real real not null,		-- seconds
  user real not null,		-- seconds
  sys real not null,		-- seconds
  rss_max integer not null	-- KB
) strict;

create view results_avg as
  select benchmark, emacs,
	 avg(real) real,
	 avg(user) user,
	 avg(sys) sys,
	 avg(rss_max) rss_max
    from results
   group by benchmark, emacs;

create view results_avg_round as
  select benchmark, emacs,
	 round(real, 2) real,
	 round(user, 2) user,
	 round(sys, 2) sys,
	 round(rss_max, 1) rss_max
    from results_avg ;

create view results_rel as
  select benchmark,
	 r.emacs emacs,
	 base.emacs base,
       	 r.real/base.real real,
	 r.user/base.user user,
	 r.sys/base.sys sys,
 	 r.rss_max/base.rss_max rss_max
    from results_avg r join results_avg base using (benchmark);

create view results_rel_diff as
  select benchmark,
	 r.emacs emacs,
	 base.emacs base,
       	 r.real/base.real - 1 real,
	 r.user/base.user - 1 user,
	 r.sys/base.sys - 1 sys,
 	 r.rss_max/base.rss_max - 1 rss_max
    from results_avg r join results_avg base using (benchmark);

create view results_rel_round as
  select benchmark, emacs, base,
	 round(real, 2) real,
	 round(user, 2) user,
	 round(sys, 2) sys,
	 round(rss_max, 2) rss_max
    from results_rel;

