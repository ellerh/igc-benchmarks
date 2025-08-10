/* Print the frequency of the time stamp counter (TSC) */

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static uint64_t
read_tsc (void)
{
  uint32_t eax, dax;
  __asm__ ("rdtsc" : "=a"(eax), "=d"(dax));
  return (uint64_t) dax << 32 | eax;
}

static __uint128_t
get_realtime_ns (void)
{
  struct timespec ts;
  if (clock_gettime (CLOCK_REALTIME, &ts) != 0)
    {
      fprintf (stderr, "clock_gettime failed: %s", strerror (errno));
      abort ();
    }
  return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

int
main (int argc, char argv[argc])
{
  __uint128_t ns_prev = 0;
  uint64_t tsc_prev = 0;

  for (size_t i = 0; i < 10; i++)
    {
      __uint128_t ns = get_realtime_ns ();
      uint64_t tsc = read_tsc ();
      uint64_t tsc_per_s
	= (tsc - tsc_prev) * 1000000000 / (uint64_t) (ns - ns_prev);
      printf ("%u: clock = %f tsc = %lu freq = %luHz\n", i,
	      (double) ns * 1e-9, tsc, tsc_per_s);
      sleep (1);
      ns_prev = ns;
      tsc_prev = tsc;
    }
  return 0;
}
