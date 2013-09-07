/*
** gptl.c
** Author: Jim Rosinski
**
** Main file contains most user-accessible GPTL functions
*/

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include <stdlib.h>        /* malloc */
#include <sys/time.h>      /* gettimeofday */
#include <sys/times.h>     /* times */
#include <unistd.h>        /* gettimeofday, syscall */
#include <stdio.h>
#include <string.h>        /* memset, strcmp (via STRMATCH) */
#include <ctype.h>         /* isdigit */
#include <sys/types.h>     /* u_int8_t, u_int16_t */
#include <math.h>          /* sqrt */
#include <assert.h>

#ifdef HAVE_PAPI
#include <papi.h>          /* PAPI_get_real_usec */
#endif

#ifdef HAVE_LIBRT
#include <time.h>
#endif

#ifdef _AIX
#include <sys/systemcfg.h>
#endif

#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#include "private.h"
#include "gptl.h"

static Timer **timers = 0;             /* linked list of timers */
static Timer **last = 0;               /* last element in list */
static int *max_depth;                 /* maximum indentation level encountered */
static int *max_name_len;              /* max length of timer name */
static volatile int nthreads = -1;     /* num threads. Init to bad value */
/* 
** For THREADED_PTHREADS case, default maxthreads to a big number.
** For THREADED_OMP, the value will be set to $OMP_NUM_THREADS, OR:
** For either case, the user can specify maxthreads with a GPTLsetoption call.
*/
#ifdef THREADED_PTHREADS
#define MAX_THREADS 64
static volatile int maxthreads = MAX_THREADS;
#else
static volatile int maxthreads = -1;   /* max threads */
#endif
static int depthlimit = 99999;         /* max depth for timers (99999 is effectively infinite) */
static volatile bool disabled = false; /* Timers disabled? */
static volatile bool initialized = false;        /* GPTLinitialize has been called */
static volatile bool pr_has_been_called = false; /* GPTLpr_file has been called */
#ifdef HAVE_PAPI
static Entry eventlist[MAX_AUX];       /* list of PAPI-based events to be counted */
static int nevents = 0;                /* number of PAPI events (init to 0) */
#endif
static bool dousepapi = false;         /* saves a function call if stays false */
static bool verbose = false;           /* output verbosity */
static bool percent = false;           /* print wallclock also as percent of 1st timers[0] */
static bool dopr_preamble = true;      /* whether to print preamble info */
static bool dopr_threadsort = true;    /* whether to print sorted thread stats */
static bool dopr_multparent = true;    /* whether to print multiple parent info */
static bool dopr_collision = true;     /* whether to print hash collision info */
static bool dopr_memusage = false;     /* whether to include memusage print when auto-profiling */

static time_t ref_gettimeofday = -1;   /* ref start point for gettimeofday */
static time_t ref_clock_gettime = -1;  /* ref start point for clock_gettime */
#ifdef _AIX
static time_t ref_read_real_time = -1; /* ref start point for read_real_time */
#endif
static long long ref_papitime = -1;    /* ref start point for PAPI_get_real_usec */

#if ( defined THREADED_OMP )

#include <omp.h>
static volatile int *threadid_omp = 0; /* array of thread ids */

#elif ( defined THREADED_PTHREADS )

#include <pthread.h>

#define MUTEX_API
#ifdef MUTEX_API
static volatile pthread_mutex_t t_mutex;
#else
static volatile pthread_mutex_t t_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif
static volatile pthread_t *threadid = 0;  /* array of thread ids */
static int lock_mutex (void);      /* lock a mutex for entry into a critical region */
static int unlock_mutex (void);    /* unlock a mutex for exit from a critical region */

#else

/* Unthreaded case */
static int threadid = -1;

#endif

typedef struct {
  const Option option;  /* wall, cpu, etc. */
  const char *str;      /* descriptive string for printing */
  bool enabled;         /* flag */
} Settings;

/* MPI summary stats */
typedef struct {
  unsigned long totcalls;  /* number of calls to the region across threads and tasks */
#ifdef HAVE_PAPI
  double papimax[MAX_AUX]; /* max counter value across threads, tasks */
  double papimin[MAX_AUX]; /* max counter value across threads, tasks */
  int papimax_p[MAX_AUX];  /* task producing papimax */
  int papimax_t[MAX_AUX];  /* thread producing papimax */
  int papimin_p[MAX_AUX];  /* task producing papimin */
  int papimin_t[MAX_AUX];  /* thread producing papimin */
#endif
  unsigned int tottsk;     /* number of tasks which invoked this region */
  float wallmax;           /* max time across threads, tasks */
  float wallmin;           /* min time across threads, tasks */
  float mean;              /* accumulated mean */
  float m2;                /* from Chan, et. al. */
  int wallmax_p;           /* task producing wallmax */
  int wallmax_t;           /* thread producing wallmax */
  int wallmin_p;           /* task producing wallmin */
  int wallmin_t;           /* thread producing wallmin */
  char name[MAX_CHARS+1];  /* timer name */
} Global;

/* Options, print strings, and default enable flags */
static Settings cpustats =      {GPTLcpu,      "Usr       sys       usr+sys   ", false};
static Settings wallstats =     {GPTLwall,     "Wallclock max       min       ", true };
static Settings overheadstats = {GPTLoverhead, "UTR_Overhead  "                , true };

static Hashentry **hashtable;    /* table of entries */
static long ticks_per_sec;       /* clock ticks per second */

typedef struct {
  int val;                       /* depth in calling tree */
  int padding[31];               /* padding is to mitigate false cache sharing */
} Nofalse; 
static Timer ***callstack;       /* call stack */
static Nofalse *stackidx;        /* index into callstack: */

static Method method = GPTLfull_tree;  /* default parent/child printing mechanism */

/* Local function prototypes */
static void printstats (const Timer *, FILE *, const int, const int, const bool, double);
static void add (Timer *, const Timer *);
static void get_threadstats (int, const char *, Global *);
static void print_multparentinfo (FILE *, Timer *);
static inline int get_cpustamp (long *, long *);
static int newchild (Timer *, Timer *);
static int get_max_depth (const Timer *, const int);
static int is_descendant (const Timer *, const Timer *);
static char *methodstr (Method);

/* Prototypes from previously separate file threadutil.c */
static int threadinit (void);                    /* initialize threading environment */
static void threadfinalize (void);               /* finalize threading environment */
static void print_threadmapping (FILE *);        /* print mapping of thread ids */
static inline int get_thread_num (void);         /* get 0-based thread number */

/* These are the (possibly) supported underlying wallclock timers */
static inline double utr_nanotime (void);
static inline double utr_mpiwtime (void);
static inline double utr_clock_gettime (void);
static inline double utr_papitime (void);
static inline double utr_read_real_time (void);
static inline double utr_gettimeofday (void);
static inline double utr_placebo (void);

static int init_nanotime (void);
static int init_mpiwtime (void);
static int init_clock_gettime (void);
static int init_papitime (void);
static int init_read_real_time (void);
static int init_gettimeofday (void);
static int init_placebo (void);

static double utr_getoverhead (void);
static inline Timer *getentry_instr (const Hashentry *, void *, unsigned int *);
static inline Timer *getentry (const Hashentry *, const char *, unsigned int *);
static void printself_andchildren (const Timer *, FILE *, const int, const int, const double);
static inline int update_parent_info (Timer *, Timer **, int);
static inline int update_stats (Timer *, const double, const long, const long, const int);
static int update_ll_hash (Timer *, const int, const unsigned int);
static inline int update_ptr (Timer *, const int);
static int construct_tree (Timer *, Method);
static int get_max_depth (const Timer *, const int);

typedef struct {
  const Funcoption option;
  double (*func)(void);
  int (*funcinit)(void);
  const char *name;
} Funcentry;

static Funcentry funclist[] = {
  {GPTLgettimeofday,   utr_gettimeofday,   init_gettimeofday,  "gettimeofday"},
  {GPTLnanotime,       utr_nanotime,       init_nanotime,      "nanotime"},
  {GPTLmpiwtime,       utr_mpiwtime,       init_mpiwtime,      "MPI_Wtime"},
  {GPTLclockgettime,   utr_clock_gettime,  init_clock_gettime, "clock_gettime"},
  {GPTLpapitime,       utr_papitime,       init_papitime,      "PAPI_get_real_usec"},
  {GPTLread_real_time, utr_read_real_time, init_read_real_time,"read_real_time"},     /* AIX only */
  {GPTLplacebo,        utr_placebo,        init_placebo,       "placebo"}      /* does nothing */
};
static const int nfuncentries = sizeof (funclist) / sizeof (Funcentry);

static double (*ptr2wtimefunc)() = 0; /* init to invalid */
static int funcidx = 0;               /* default timer is gettimeofday */

#ifdef HAVE_NANOTIME
static float cpumhz = -1.;                        /* init to bad value */
static double cyc2sec = -1;                       /* init to bad value */
static inline long long nanotime (void);          /* read counter (assembler) */
static float get_clockfreq (void);                /* cycles/sec */
static char *clock_source = "UNKNOWN";            /* where clock found */
#endif

#define DEFAULT_TABLE_SIZE 1023
static int tablesize = DEFAULT_TABLE_SIZE;  /* per-thread size of hash table (settable parameter) */
static char *outdir = 0;                    /* dir to write output files to (currently unused) */

#define MSGSIZ 64                           /* max size of msg printed when dopr_memusage=true */

/* VERBOSE is a debugging ifdef local to the rest of this file */
#undef VERBOSE

/*
** GPTLsetoption: set option value to true or false.
**
** Input arguments:
**   option: option to be set
**   val:    value to which option should be set (nonzero=true, zero=false)
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLsetoption (const int option,  /* option */
                   const int val)     /* value */
{
  static const char *thisfunc = "GPTLsetoption";

  if (initialized)
    return GPTLerror ("%s: must be called BEFORE GPTLinitialize\n", thisfunc);

  if (option == GPTLabort_on_error) {
    GPTLset_abort_on_error ((bool) val);
    if (verbose)
      printf ("%s: boolean abort_on_error = %d\n", thisfunc, val);
    return 0;
  }

  switch (option) {
  case GPTLcpu:
#ifdef HAVE_TIMES
    cpustats.enabled = (bool) val; 
    if (verbose)
      printf ("%s: cpustats = %d\n", thisfunc, val);
#else
    if (val)
      return GPTLerror ("%s: times() not available\n", thisfunc);
#endif
    return 0;
  case GPTLwall:     
    wallstats.enabled = (bool) val; 
    if (verbose)
      printf ("%s: boolean wallstats = %d\n", thisfunc, val);
    return 0;
  case GPTLoverhead: 
    overheadstats.enabled = (bool) val; 
    if (verbose)
      printf ("%s: boolean overheadstats = %d\n", thisfunc, val);
    return 0;
  case GPTLdepthlimit: 
    depthlimit = val; 
    if (verbose)
      printf ("%s: depthlimit = %d\n", thisfunc, val);
    return 0;
  case GPTLverbose: 
    verbose = (bool) val; 
#ifdef HAVE_PAPI
    (void) GPTL_PAPIsetoption (GPTLverbose, val);
#endif
    if (verbose)
      printf ("%s: boolean verbose = %d\n", thisfunc, val);
    return 0;
  case GPTLpercent: 
    percent = (bool) val; 
    if (verbose)
      printf ("%s: boolean percent = %d\n", thisfunc, val);
    return 0;
  case GPTLdopr_preamble: 
    dopr_preamble = (bool) val; 
    if (verbose)
      printf ("%s: boolean dopr_preamble = %d\n", thisfunc, val);
    return 0;
  case GPTLdopr_threadsort: 
    dopr_threadsort = (bool) val; 
    if (verbose)
      printf ("%s: boolean dopr_threadsort = %d\n", thisfunc, val);
    return 0;
  case GPTLdopr_multparent: 
    dopr_multparent = (bool) val; 
    if (verbose)
      printf ("%s: boolean dopr_multparent = %d\n", thisfunc, val);
    return 0;
  case GPTLdopr_collision: 
    dopr_collision = (bool) val; 
    if (verbose)
      printf ("%s: boolean dopr_collision = %d\n", thisfunc, val);
    return 0;
  case GPTLdopr_memusage: 
    dopr_memusage = (bool) val; 
    if (verbose)
      printf ("%s: boolean dopr_memusage = %d\n", thisfunc, val);
    return 0;
  case GPTLprint_method:
    method = (Method) val; 
    if (verbose)
      printf ("%s: print_method = %s\n", thisfunc, methodstr (method));
    return 0;
  case GPTLtablesize:
    if (val < 1)
      return GPTLerror ("%s: tablesize must be positive. %d is invalid\n", thisfunc, val);

    tablesize = val;
    if (verbose)
      printf ("%s: tablesize = %d\n", thisfunc, tablesize);
    return 0;
  case GPTLsync_mpi:
#ifdef ENABLE_PMPI
    if (GPTLpmpi_setoption (option, val) != 0)
      fprintf (stderr, "%s: GPTLpmpi_setoption failure\n", thisfunc);
#endif
    if (verbose)
      printf ("%s: boolean sync_mpi = %d\n", thisfunc, val);
    return 0;

  case GPTLmaxthreads:
    if (val < 1)
      return GPTLerror ("%s: maxthreads must be positive. %d is invalid\n", thisfunc, val);

    maxthreads = val;
    return 0;
    
  case GPTLmultiplex:
    /* Allow GPTLmultiplex to fall through because it will be handled by GPTL_PAPIsetoption() */
  default:
    break;
  }

#ifdef HAVE_PAPI
  if (GPTL_PAPIsetoption (option, val) == 0) {
    if (val)
      dousepapi = true;
    return 0;
  }
#else
  /* Make GPTLnarrowprint a placebo if PAPI not enabled */
  if (option == GPTLnarrowprint)
    return 0;
#endif

  return GPTLerror ("%s: faiure to enable option %d\n", thisfunc, option);
}

/*
** GPTLsetutr: set underlying timing routine.
**
** Input arguments:
**   option: index which sets function
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLsetutr (const int option)
{
  int i;  /* index over number of underlying timer  */
  static const char *thisfunc = "GPTLsetutr";

  if (initialized)
    return GPTLerror ("%s: must be called BEFORE GPTLinitialize\n", thisfunc);

  for (i = 0; i < nfuncentries; i++) {
    if (option == (int) funclist[i].option) {
      if (verbose)
        printf ("%s: underlying wallclock timer = %s\n", thisfunc, funclist[i].name);
      funcidx = i;

      /*
      ** Return an error condition if the function is not available.
      ** OK for the user code to ignore: GPTLinitialize() will reset to gettimeofday
      */

      if ((*funclist[i].funcinit)() < 0)
        return GPTLerror ("%s: utr=%s not available or doesn't work\n", thisfunc, funclist[i].name);
      else
        return 0;
    }
  }
  return GPTLerror ("%s: unknown option %d\n", thisfunc, option);
}

/*
** GPTLinitialize (): Initialization routine must be called from single-threaded
**   region before any other timing routines may be called.  The need for this
**   routine could be eliminated if not targetting timing library for threaded
**   capability. 
**
** return value: 0 (success) or GPTLerror (failure)
*/
int GPTLinitialize (void)
{
  int i;          /* loop index */
  int t;          /* thread index */
  double t1, t2;  /* returned from underlying timer */
  static const char *thisfunc = "GPTLinitialize";

  if (initialized)
    return GPTLerror ("%s: has already been called\n", thisfunc);

  if (threadinit () < 0)
    return GPTLerror ("%s: bad return from threadinit\n", thisfunc);

  if ((ticks_per_sec = sysconf (_SC_CLK_TCK)) == -1)
    return GPTLerror ("%s: failure from sysconf (_SC_CLK_TCK)\n", thisfunc);

  /* Allocate space for global arrays */
  callstack     = (Timer ***)    GPTLallocate (maxthreads * sizeof (Timer **));
  stackidx      = (Nofalse *)    GPTLallocate (maxthreads * sizeof (Nofalse));
  timers        = (Timer **)     GPTLallocate (maxthreads * sizeof (Timer *));
  last          = (Timer **)     GPTLallocate (maxthreads * sizeof (Timer *));
  max_depth     = (int *)        GPTLallocate (maxthreads * sizeof (int));
  max_name_len  = (int *)        GPTLallocate (maxthreads * sizeof (int));
  hashtable     = (Hashentry **) GPTLallocate (maxthreads * sizeof (Hashentry *));

  /* Initialize array values */
  for (t = 0; t < maxthreads; t++) {
    max_depth[t]    = -1;
    max_name_len[t] = 0;
    callstack[t] = (Timer **) GPTLallocate (MAX_STACK * sizeof (Timer *));
    hashtable[t] = (Hashentry *) GPTLallocate (tablesize * sizeof (Hashentry));
    for (i = 0; i < tablesize; i++) {
      hashtable[t][i].nument = 0;
      hashtable[t][i].entries = 0;
    }

    /* Make a timer "GPTL_ROOT" to ensure no orphans, and to simplify printing. */
    timers[t] = (Timer *) GPTLallocate (sizeof (Timer));
    memset (timers[t], 0, sizeof (Timer));
    strcpy (timers[t]->name, "GPTL_ROOT");
    timers[t]->onflg = true;
    last[t] = timers[t];

    stackidx[t].val = 0;
    callstack[t][0] = timers[t];
    for (i = 1; i < MAX_STACK; i++)
      callstack[t][i] = 0;
  }

#ifdef HAVE_PAPI
  if (GPTL_PAPIinitialize (maxthreads, verbose, &nevents, eventlist) < 0)
    return GPTLerror ("%s: Failure from GPTL_PAPIinitialize\n", thisfunc);
#endif

  /* Call init routine for underlying timing routine. */
  if ((*funclist[funcidx].funcinit)() < 0) {
    fprintf (stderr, "%s: Failure initializing %s. Reverting underlying timer to %s\n", 
             thisfunc, funclist[funcidx].name, funclist[0].name);
    funcidx = 0;
  }

  ptr2wtimefunc = funclist[funcidx].func;

  if (verbose) {
    t1 = (*ptr2wtimefunc) ();
    t2 = (*ptr2wtimefunc) ();
    if (t1 > t2)
      fprintf (stderr, "%s: negative delta-t=%g\n", thisfunc, t2-t1);

    printf ("Per call overhead est. t2-t1=%g should be near zero\n", t2-t1);
    printf ("Underlying wallclock timing routine is %s\n", funclist[funcidx].name);
  }

  initialized = true;
  return 0;
}

/*
** GPTLfinalize (): Finalization routine must be called from single-threaded
**   region. Free all malloc'd space
**
** return value: 0 (success) or GPTLerror (failure)
*/
int GPTLfinalize (void)
{
  int t;                /* thread index */
  int n;                /* array index */
  Timer *ptr, *ptrnext; /* ll indices */
  static const char *thisfunc = "GPTLfinalize";

  if ( ! initialized)
    return GPTLerror ("%s: initialization was not completed\n", thisfunc);

  for (t = 0; t < maxthreads; ++t) {
    for (n = 0; n < tablesize; ++n) {
      if (hashtable[t][n].nument > 0)
        free (hashtable[t][n].entries);
    }
    free (hashtable[t]);
    hashtable[t] = NULL;
    free (callstack[t]);
    for (ptr = timers[t]; ptr; ptr = ptrnext) {
      ptrnext = ptr->next;
      if (ptr->nparent > 0) {
        free (ptr->parent);
        free (ptr->parent_count);
      }
      if (ptr->nchildren > 0)
        free (ptr->children);
      free (ptr);
    }
  }

  free (callstack);
  free (stackidx);
  free (timers);
  free (last);
  free (max_depth);
  free (max_name_len);
  free (hashtable);

  threadfinalize ();

#ifdef HAVE_PAPI
  GPTL_PAPIfinalize (maxthreads);
#endif

  /* Reset initial values */
  timers = 0;
  last = 0;
  max_depth = 0;
  max_name_len = 0;
  nthreads = -1;
#ifdef THREADED_PTHREADS
  maxthreads = MAX_THREADS;
#else
  maxthreads = -1;
#endif
  depthlimit = 99999;
  disabled = false;
  initialized = false;
  pr_has_been_called = false;
  dousepapi = false;
  verbose = false;
  percent = false;
  dopr_preamble = true;
  dopr_threadsort = true;
  dopr_multparent = true;
  dopr_collision = true;
  ref_gettimeofday = -1;
  ref_clock_gettime = -1;
#ifdef _AIX
  ref_read_real_time = -1;
#endif
  ref_papitime = -1;
  funcidx = 0;
#ifdef HAVE_NANOTIME
  cpumhz= 0;
  cyc2sec = -1;
#endif
  outdir = 0;
  tablesize = DEFAULT_TABLE_SIZE;

  return 0;
}

/*
** GPTLstart_instr: start a timer (auto-instrumented)
**
** Input arguments:
**   self: function address
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLstart_instr (void *self)
{
  Timer *ptr;              /* linked list pointer */
  int t;                   /* thread index (of this thread) */
  unsigned int indx;       /* hash table index */
  static const char *thisfunc = "GPTLstart_instr";

  if (disabled)
    return 0;

  if ( ! initialized)
    return GPTLerror ("%s self=%p: GPTLinitialize has not been called\n", thisfunc, self);

  if ((t = get_thread_num ()) < 0)
    return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);

  /* If current depth exceeds a user-specified limit for print, just increment and return */
  if (stackidx[t].val >= depthlimit) {
    ++stackidx[t].val;
    return 0;
  }

  ptr = getentry_instr (hashtable[t], self, &indx);

  /* 
  ** Recursion => increment depth in recursion and return.  We need to return 
  ** because we don't want to restart the timer.  We want the reported time for
  ** the timer to reflect the outermost layer of recursion.
  */
  if (ptr && ptr->onflg) {
    ++ptr->recurselvl;
    return 0;
  }

  /*
  ** Increment stackidx[t] unconditionally. This is necessary to ensure the correct
  ** behavior when GPTLstop_instr decrements stackidx[t] unconditionally.
  */
  if (++stackidx[t].val > MAX_STACK-1)
    return GPTLerror ("%s: stack too big\n", thisfunc);

  if ( ! ptr) {     /* Add a new entry and initialize */
    ptr = (Timer *) GPTLallocate (sizeof (Timer));
    memset (ptr, 0, sizeof (Timer));

    /*
    ** Need to save the address string for later conversion back to a real
    ** name by an offline tool.
    */
    snprintf (ptr->name, MAX_CHARS+1, "%lx", (unsigned long) self);
    ptr->address = self;

    if (update_ll_hash (ptr, t, indx) != 0)
      return GPTLerror ("%s: update_ll_hash error\n", thisfunc);
  }

  if (update_parent_info (ptr, callstack[t], stackidx[t].val) != 0)
    return GPTLerror ("%s: update_parent_info error\n", thisfunc);

  if (update_ptr (ptr, t) != 0)
    return GPTLerror ("%s: update_ptr error\n", thisfunc);

  return (0);
}  

/*
** GPTLstart: start a timer
**
** Input arguments:
**   name: timer name
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLstart (const char *name)               /* timer name */
{
  Timer *ptr;        /* linked list pointer */
  int t;             /* thread index (of this thread) */
  int numchars;      /* number of characters to copy */
  unsigned int indx; /* hash table index */
  static const char *thisfunc = "GPTLstart";

  if (disabled)
    return 0;

  if ( ! initialized)
    return GPTLerror ("%s name=%s: GPTLinitialize has not been called\n", thisfunc, name);

  if ((t = get_thread_num ()) < 0)
    return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);

  /*
  ** If current depth exceeds a user-specified limit for print, just
  ** increment and return
  */

  if (stackidx[t].val >= depthlimit) {
    ++stackidx[t].val;
    return 0;
  }

  /* ptr will point to the requested timer in the current list, or NULL if this is a new entry */
  ptr = getentry (hashtable[t], name, &indx);

  /* 
  ** Recursion => increment depth in recursion and return.  We need to return 
  ** because we don't want to restart the timer.  We want the reported time for
  ** the timer to reflect the outermost layer of recursion.
  */
  if (ptr && ptr->onflg) {
    ++ptr->recurselvl;
    return 0;
  }

  /*
  ** Increment stackidx[t] unconditionally. This is necessary to ensure the correct
  ** behavior when GPTLstop decrements stackidx[t] unconditionally.
  */
  if (++stackidx[t].val > MAX_STACK-1)
    return GPTLerror ("%s: stack too big\n", thisfunc);

  if ( ! ptr) { /* Add a new entry and initialize */
    ptr = (Timer *) GPTLallocate (sizeof (Timer));
    memset (ptr, 0, sizeof (Timer));

    numchars = MIN (strlen (name), MAX_CHARS);
    strncpy (ptr->name, name, numchars);
    ptr->name[numchars] = '\0';

    if (update_ll_hash (ptr, t, indx) != 0)
      return GPTLerror ("%s: update_ll_hash error\n", thisfunc);
  }

  if (update_parent_info (ptr, callstack[t], stackidx[t].val) != 0)
    return GPTLerror ("%s: update_parent_info error\n", thisfunc);

  if (update_ptr (ptr, t) != 0)
    return GPTLerror ("%s: update_ptr error\n", thisfunc);

  return (0);
}

/*
** GPTLstart_handle: start a timer based on a handle
**
** Input arguments:
**   name: timer name (required when on input, handle=0)
**   handle: pointer to timer matching "name"
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLstart_handle (const char *name,  /* timer name */
                      void **handle)     /* handle (output if input value is 0) */
{
  Timer *ptr;                            /* linked list pointer */
  int t;                                 /* thread index (of this thread) */
  int numchars;                          /* number of characters to copy */
  unsigned int indx = (unsigned int) -1; /* hash table index: init to bad value */
  static const char *thisfunc = "GPTLstart_handle";

  if (disabled)
    return 0;

  if ( ! initialized)
    return GPTLerror ("%s name=%s: GPTLinitialize has not been called\n", thisfunc, name);

  if ((t = get_thread_num ()) < 0)
    return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);

  /* If current depth exceeds a user-specified limit for print, just increment and return */
  if (stackidx[t].val >= depthlimit) {
    ++stackidx[t].val;
    return 0;
  }

  /*
  ** If on input, handle references a non-zero value, assume it's a previously returned Timer* 
  ** passed in by the user. If zero, generate the hash entry and return it to the user.
  */
  if (*handle) {
    ptr = (Timer *) *handle;
  } else {
    ptr = getentry (hashtable[t], name, &indx);
  }
    
  /* 
  ** Recursion => increment depth in recursion and return.  We need to return 
  ** because we don't want to restart the timer.  We want the reported time for
  ** the timer to reflect the outermost layer of recursion.
  */
  if (ptr && ptr->onflg) {
    ++ptr->recurselvl;
    return 0;
  }

  /*
  ** Increment stackidx[t] unconditionally. This is necessary to ensure the correct
  ** behavior when GPTLstop decrements stackidx[t] unconditionally.
  */
  if (++stackidx[t].val > MAX_STACK-1)
    return GPTLerror ("%s: stack too big\n", thisfunc);

  if ( ! ptr) { /* Add a new entry and initialize */
    ptr = (Timer *) GPTLallocate (sizeof (Timer));
    memset (ptr, 0, sizeof (Timer));

    numchars = MIN (strlen (name), MAX_CHARS);
    strncpy (ptr->name, name, numchars);
    ptr->name[numchars] = '\0';

    if (update_ll_hash (ptr, t, indx) != 0)
      return GPTLerror ("%s: update_ll_hash error\n", thisfunc);
  }

  if (update_parent_info (ptr, callstack[t], stackidx[t].val) != 0)
    return GPTLerror ("%s: update_parent_info error\n", thisfunc);

  if (update_ptr (ptr, t) != 0)
    return GPTLerror ("%s: update_ptr error\n", thisfunc);

  /* If on input, *handle was 0, return the pointer to the timer for future input */
  if ( ! *handle)
    *handle = (void *) ptr;

  return (0);
}

/*
** update_ll_hash: Update linked list and hash table.
**                 Called by GPTLstart, GPTLstart_instr and GPTLstart_handle
**
** Input arguments:
**   ptr:  pointer to timer
**   t:    thread index
**   indx: hash index
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static int update_ll_hash (Timer *ptr, const int t, const unsigned int indx)
{
  int nchars;      /* number of chars */
  int nument;      /* number of entries */
  Timer **eptr;    /* for realloc */

  nchars = strlen (ptr->name);
  if (nchars > max_name_len[t])
    max_name_len[t] = nchars;

  last[t]->next = ptr;
  last[t] = ptr;
  ++hashtable[t][indx].nument;
  nument = hashtable[t][indx].nument;
  
  eptr = (Timer **) realloc (hashtable[t][indx].entries, nument * sizeof (Timer *));
  if ( ! eptr)
    return GPTLerror ("update_ll_hash: realloc error\n");

  hashtable[t][indx].entries           = eptr;
  hashtable[t][indx].entries[nument-1] = ptr;

  return 0;
}

/*
** update_ptr: Update timer contents. Called by GPTLstart and GPTLstart_instr and GPTLstart_handle
**
** Input arguments:
**   ptr:  pointer to timer
**   t:    thread index
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static inline int update_ptr (Timer *ptr, const int t)
{
  double tp2;    /* time stamp */

  ptr->onflg = true;

  if (cpustats.enabled && get_cpustamp (&ptr->cpu.last_utime, &ptr->cpu.last_stime) < 0)
    return GPTLerror ("update_ptr: get_cpustamp error");
  
  if (wallstats.enabled) {
    tp2 = (*ptr2wtimefunc) ();
    ptr->wall.last = tp2;
  }

#ifdef HAVE_PAPI
  if (dousepapi && GPTL_PAPIstart (t, &ptr->aux) < 0)
    return GPTLerror ("update_ptr: error from GPTL_PAPIstart\n");
#endif
  return 0;
}

/*
** update_parent_info: update info about parent, and in the parent about this child
**
** Arguments:
**   ptr:  pointer to timer
**   callstackt: callstack for this thread
**   stackidxt:  stack index for this thread
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static inline int update_parent_info (Timer *ptr, 
                                      Timer **callstackt, 
                                      int stackidxt) 
{
  int n;             /* loop index through known parents */
  Timer *pptr;       /* pointer to parent in callstack */
  Timer **pptrtmp;   /* for realloc parent pointer array */
  int nparent;       /* number of parents */
  int *parent_count; /* number of times parent invoked this child */
  static const char *thisfunc = "update_parent_info";

  if ( ! ptr )
    return -1;

  if (stackidxt < 0)
    return GPTLerror ("%s: called with negative stackidx\n", thisfunc);

  callstackt[stackidxt] = ptr;

  /* If the region has no parent, bump its orphan count (should never happen since "GPTL_ROOT" added) */
  if (stackidxt == 0) {
    ++ptr->norphan;
    return 0;
  }

  pptr = callstackt[stackidxt-1];

  /* If this parent occurred before, bump its count */
  for (n = 0; n < ptr->nparent; ++n) {
    if (ptr->parent[n] == pptr) {
      ++ptr->parent_count[n];
      break;
    }
  }

  /* If this is a new parent, update info */
  if (n == ptr->nparent) {
    ++ptr->nparent;
    nparent = ptr->nparent;
    pptrtmp = (Timer **) realloc (ptr->parent, nparent * sizeof (Timer *));
    if ( ! pptrtmp)
      return GPTLerror ("%s: realloc error pptrtmp nparent=%d\n", thisfunc, nparent);

    ptr->parent = pptrtmp;
    ptr->parent[nparent-1] = pptr;
    parent_count = (int *) realloc (ptr->parent_count, nparent * sizeof (int));
    if ( ! parent_count)
      return GPTLerror ("%s: realloc error parent_count nparent=%d\n", thisfunc, nparent);

    ptr->parent_count = parent_count;
    ptr->parent_count[nparent-1] = 1;
  }

  return 0;
}

/*
** GPTLstop_instr: stop a timer (auto-instrumented)
**
** Input arguments:
**   self: function address
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLstop_instr (void *self)
{
  double tp1 = 0.0;          /* time stamp */
  Timer *ptr;                /* linked list pointer */
  int t;                     /* thread number for this process */
  unsigned int indx;         /* index into hash table */
  long usr = 0;              /* user time (returned from get_cpustamp) */
  long sys = 0;              /* system time (returned from get_cpustamp) */
  static const char *thisfunc = "GPTLstop_instr";

  if (disabled)
    return 0;

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);

  /* Get the timestamp */    
  if (wallstats.enabled) {
    tp1 = (*ptr2wtimefunc) ();
  }

  if (cpustats.enabled && get_cpustamp (&usr, &sys) < 0)
    return GPTLerror ("%s: bad return from get_cpustamp\n", thisfunc);

  if ((t = get_thread_num ()) < 0)
    return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);

  /* If current depth exceeds a user-specified limit for print, just decrement and return */
  if (stackidx[t].val > depthlimit) {
    --stackidx[t].val;
    return 0;
  }

  ptr = getentry_instr (hashtable[t], self, &indx);

  if ( ! ptr) 
    return GPTLerror ("%s: timer for %p had not been started.\n", thisfunc, self);

  if ( ! ptr->onflg )
    return GPTLerror ("%s: timer %s was already off.\n", thisfunc, ptr->name);

  ++ptr->count;

  /* 
  ** Recursion => decrement depth in recursion and return.  We need to return
  ** because we don't want to stop the timer.  We want the reported time for
  ** the timer to reflect the outermost layer of recursion.
  */
  if (ptr->recurselvl > 0) {
    ++ptr->nrecurse;
    --ptr->recurselvl;
    return 0;
  }

  if (update_stats (ptr, tp1, usr, sys, t) != 0)
    return GPTLerror ("%s: error from update_stats\n", thisfunc);

  return 0;
}

/*
** GPTLstop: stop a timer
**
** Input arguments:
**   name: timer name
**
** Return value: 0 (success) or -1 (failure)
*/
int GPTLstop (const char *name)               /* timer name */
{
  double tp1 = 0.0;          /* time stamp */
  Timer *ptr;                /* linked list pointer */
  int t;                     /* thread number for this process */
  unsigned int indx;         /* index into hash table */
  long usr = 0;              /* user time (returned from get_cpustamp) */
  long sys = 0;              /* system time (returned from get_cpustamp) */
  static const char *thisfunc = "GPTLstop";

  if (disabled)
    return 0;

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);

  /* Get the timestamp */
    
  if (wallstats.enabled) {
    tp1 = (*ptr2wtimefunc) ();
  }

  if (cpustats.enabled && get_cpustamp (&usr, &sys) < 0)
    return GPTLerror ("%s: get_cpustamp error", thisfunc);

  if ((t = get_thread_num ()) < 0)
    return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);

  /* If current depth exceeds a user-specified limit for print, just decrement and return */
  if (stackidx[t].val > depthlimit) {
    --stackidx[t].val;
    return 0;
  }

  if ( ! (ptr = getentry (hashtable[t], name, &indx)))
    return GPTLerror ("%s thread %d: timer for %s had not been started.\n", thisfunc, t, name);

  if ( ! ptr->onflg )
    return GPTLerror ("%s: timer %s was already off.\n", thisfunc, ptr->name);

  ++ptr->count;

  /* 
  ** Recursion => decrement depth in recursion and return.  We need to return
  ** because we don't want to stop the timer.  We want the reported time for
  ** the timer to reflect the outermost layer of recursion.
  */
  if (ptr->recurselvl > 0) {
    ++ptr->nrecurse;
    --ptr->recurselvl;
    return 0;
  }

  if (update_stats (ptr, tp1, usr, sys, t) != 0)
    return GPTLerror ("%s: error from update_stats\n", thisfunc);

  return 0;
}

/*
** GPTLstop_handle: stop a timer based on a handle
**
** Input arguments:
**   name: timer name (used only for diagnostics)
**   handle: pointer to timer
**
** Return value: 0 (success) or -1 (failure)
*/
int GPTLstop_handle (const char *name,     /* timer name */
                     void **handle)        /* handle */
{
  double tp1 = 0.0;          /* time stamp */
  Timer *ptr;                /* linked list pointer */
  int t;                     /* thread number for this process */
  long usr = 0;              /* user time (returned from get_cpustamp) */
  long sys = 0;              /* system time (returned from get_cpustamp) */
  static const char *thisfunc = "GPTLstop_handle";

  if (disabled)
    return 0;

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);

  /* Get the timestamp */
    
  if (wallstats.enabled) {
    tp1 = (*ptr2wtimefunc) ();
  }

  if (cpustats.enabled && get_cpustamp (&usr, &sys) < 0)
    return GPTLerror (0);

  if ((t = get_thread_num ()) < 0)
    return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);

  /* If current depth exceeds a user-specified limit for print, just decrement and return */
  if (stackidx[t].val > depthlimit) {
    --stackidx[t].val;
    return 0;
  }

  if ( ! *handle) 
    return GPTLerror ("%s: bad input handle for timer %s.\n", thisfunc, name);
    
  ptr = (Timer *) *handle;

  if ( ! ptr->onflg )
    return GPTLerror ("%s: timer %s was already off.\n", thisfunc, ptr->name);

  ++ptr->count;

  /* 
  ** Recursion => decrement depth in recursion and return.  We need to return
  ** because we don't want to stop the timer.  We want the reported time for
  ** the timer to reflect the outermost layer of recursion.
  */
  if (ptr->recurselvl > 0) {
    ++ptr->nrecurse;
    --ptr->recurselvl;
    return 0;
  }

  if (update_stats (ptr, tp1, usr, sys, t) != 0)
    return GPTLerror ("%s: error from update_stats\n", thisfunc);

  return 0;
}

/*
** update_stats: update stats inside ptr. Called by GPTLstop, GPTLstop_instr, 
**               GPTLstop_handle
**
** Input arguments:
**   ptr: pointer to timer
**   tp1: input time stapm
**   usr: user time
**   sys: system time
**   t: thread index
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static inline int update_stats (Timer *ptr, 
                                const double tp1, 
                                const long usr, 
                                const long sys,
                                const int t)
{
  double delta;       /* difference */
  static const char *thisfunc = "update_stats";

  ptr->onflg = false;
  --stackidx[t].val;
  if (stackidx[t].val < -1) {
    stackidx[t].val = -1;
    return GPTLerror ("%s: tree depth has become negative.\n", thisfunc);
  }

#ifdef HAVE_PAPI
  if (dousepapi && GPTL_PAPIstop (t, &ptr->aux) < 0)
    return GPTLerror ("%s: error from GPTL_PAPIstop\n", thisfunc);
#endif

  if (wallstats.enabled) {
    delta = tp1 - ptr->wall.last;
    ptr->wall.accum += delta;

    if (delta < 0.) {
      fprintf (stderr, "%s: negative delta=%g\n", thisfunc, delta);
    }

    if (ptr->count == 1) {
      ptr->wall.max = delta;
      ptr->wall.min = delta;
    } else {
      if (delta > ptr->wall.max)
        ptr->wall.max = delta;
      if (delta < ptr->wall.min)
        ptr->wall.min = delta;
    }
  }

  if (cpustats.enabled) {
    ptr->cpu.accum_utime += usr - ptr->cpu.last_utime;
    ptr->cpu.accum_stime += sys - ptr->cpu.last_stime;
    ptr->cpu.last_utime   = usr;
    ptr->cpu.last_stime   = sys;
  }
  return 0;
}

/*
** GPTLenable: enable timers
**
** Return value: 0 (success)
*/
int GPTLenable (void)
{
  disabled = false;
  return (0);
}

/*
** GPTLdisable: disable timers
**
** Return value: 0 (success)
*/
int GPTLdisable (void)
{
  disabled = true;
  return (0);
}

/*
** GPTLstamp: Compute timestamp of usr, sys, and wallclock time (seconds)
**
** Output arguments:
**   wall: wallclock
**   usr:  user time
**   sys:  system time
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLstamp (double *wall, double *usr, double *sys)
{
  struct tms buf;            /* argument to times */

  if ( ! initialized)
    return GPTLerror ("GPTLstamp: GPTLinitialize has not been called\n");

#ifdef HAVE_TIMES
  *usr = 0;
  *sys = 0;

  if (times (&buf) == -1)
    return GPTLerror ("GPTLstamp: times() failed. Results bogus\n");

  *usr = buf.tms_utime / (double) ticks_per_sec;
  *sys = buf.tms_stime / (double) ticks_per_sec;
#endif
  *wall = (*ptr2wtimefunc) ();
  return 0;
}

/*
** GPTLreset: reset all timers to 0
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLreset (void)
{
  int t;             /* index over threads */
  Timer *ptr;        /* linked list index */
  static const char *thisfunc = "GPTLreset";

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);

  for (t = 0; t < nthreads; t++) {
    for (ptr = timers[t]; ptr; ptr = ptr->next) {
      ptr->onflg = false;
      ptr->count = 0;
      memset (&ptr->wall, 0, sizeof (ptr->wall));
      memset (&ptr->cpu, 0, sizeof (ptr->cpu));
#ifdef HAVE_PAPI
      memset (&ptr->aux, 0, sizeof (ptr->aux));
#endif
    }
  }

  if (verbose)
    printf ("%s: accumulators for all timers set to zero\n", thisfunc);

  return 0;
}

/* 
** GPTLpr: Print values of all timers
**
** Input arguments:
**   id: integer to append to string "timing."
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLpr (const int id)   /* output file will be named "timing.<id>" */
{
  char outfile[14];         /* name of output file: timing.xxxxxx */
  static const char *thisfunc = "GPTLpr";

  if (id < 0 || id > 999999)
    return GPTLerror ("%s: bad id=%d for output file. Must be >= 0 and < 1000000\n", thisfunc, id);

  sprintf (outfile, "timing.%d", id);

  if (GPTLpr_file (outfile) != 0)
    return GPTLerror ("%s: Error in GPTLpr_file\n", thisfunc);

  return 0;
}

/* 
** GPTLpr_file: Print values of all timers
**
** Input arguments:
**   outfile: Name of output file to write
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int GPTLpr_file (const char *outfile) /* output file to write */
{
  FILE *fp;                 /* file handle to write to */
  Timer *ptr;               /* walk through master thread linked list */
  Timer *tptr;              /* walk through slave threads linked lists */
  Timer sumstats;           /* sum of same timer stats over threads */
  int i, ii, n, t;          /* indices */
  int totent;               /* per-thread collision count (diagnostic) */
  int nument;               /* per-index collision count (diagnostic) */
  int totlen;               /* length for malloc */
  unsigned long totcount;   /* total timer invocations */
  char *outpath;            /* path to output file: outdir/timing.xxxxxx */
  float *sum;               /* sum of overhead values (per thread) */
  float osum;               /* sum of overhead over threads */
  double utr_overhead;      /* overhead of calling underlying timing routine */
  double tot_overhead;      /* utr_overhead + papi overhead */
  double papi_overhead = 0; /* overhead of reading papi counters */
  bool found;               /* jump out of loop when name found */
  bool foundany;            /* whether summation print necessary */
  bool first;               /* flag 1st time entry found */
  /*
  ** Diagnostics for collisions and GPTL memory usage
  */
  int num_zero;             /* number of buckets with 0 collisions */
  int num_one;              /* number of buckets with 1 collision */
  int num_two;              /* number of buckets with 2 collisions */
  int num_more;             /* number of buckets with more than 2 collisions */
  int most;                 /* biggest collision count */
  int numtimers;            /* number of timers */
  float hashmem;            /* hash table memory usage */
  float regionmem;          /* timer memory usage */
  float papimem;            /* PAPI stats memory usage */
  float pchmem;             /* parent/child array memory usage */
  float totmem;             /* total GPTL memory usage */

  static const char *thisfunc = "GPTLpr_file";

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize() has not been called\n", thisfunc);

  /* 2 is for "/" plus null */
  if (outdir)
    totlen = strlen (outdir) + strlen (outfile) + 2; 
  else
    totlen = strlen (outfile) + 2; 

  outpath = (char *) GPTLallocate (totlen);

  if (outdir) {
     strcpy (outpath, outdir);
     strcat (outpath, "/");
     strcat (outpath, outfile);
  } else {
     strcpy (outpath, outfile);
  }

  if ( ! (fp = fopen (outpath, "w")))
    fp = stderr;

  free (outpath);

  /* A set of nasty ifdefs to tell important aspects of how GPTL was built */
#ifdef HAVE_NANOTIME
  if (funclist[funcidx].option == GPTLnanotime) {
    fprintf (fp, "Clock rate = %f MHz\n", cpumhz);
    fprintf (fp, "Source of clock rate was %s\n", clock_source);
    if (strcmp (clock_source, "/proc/cpuinfo") == 0) {
      fprintf (fp, "WARNING: The contents of /proc/cpuinfo can change in variable frequency CPUs");
      fprintf (fp, "Therefore the use of nanotime (register read) is not recommended on machines so equipped");
    }
#ifdef BIT64
    fprintf (fp, "  BIT64 was true\n");
#else
    fprintf (fp, "  BIT64 was false\n");
#endif
  }
#endif

#if ( defined THREADED_OMP )
  fprintf (fp, "GPTL was built with THREADED_OMP\n");
#elif ( defined THREADED_PTHREADS )
  fprintf (fp, "GPTL was built with THREADED_PTHREADS\n");
#else
  fprintf (fp, "GPTL was built without threading\n");
#endif

#ifdef HAVE_MPI
  fprintf (fp, "HAVE_MPI was true\n");

#ifdef HAVE_COMM_F2C
  fprintf (fp, "  HAVE_COMM_F2C was true\n");
#else
  fprintf (fp, "  HAVE_COMM_F2C was false\n");
#endif

#ifdef ENABLE_PMPI
  fprintf (fp, "  ENABLE_PMPI was true\n");
#else
  fprintf (fp, "  ENABLE_PMPI was false\n");
#endif

#else
  fprintf (fp, "HAVE_MPI was false\n");
#endif

#ifdef HAVE_PAPI
  fprintf (fp, "HAVE_PAPI was true\n");
  if (dousepapi) {
    if (GPTL_PAPIis_multiplexed ())
      fprintf (fp, "  PAPI event multiplexing was ON\n");
    else
      fprintf (fp, "  PAPI event multiplexing was OFF\n");
    GPTL_PAPIprintenabled (fp);
  }
#else
  fprintf (fp, "HAVE_PAPI was false\n");
#endif

  /* Estimate underlying timing routine overhead */
  utr_overhead = utr_getoverhead ();
  fprintf (fp, "Underlying timing routine was %s.\n", funclist[funcidx].name);
  fprintf (fp, "Per-call utr overhead est: %g sec.\n", utr_overhead);
#ifdef HAVE_PAPI
  if (dousepapi) {
    double t1, t2;
    t1 = (*ptr2wtimefunc) ();
    read_counters100 ();
    t2 = (*ptr2wtimefunc) ();
    papi_overhead = 0.01 * (t2 - t1);
    fprintf (fp, "Per-call PAPI overhead est: %g sec.\n", papi_overhead);
  }
#endif
  tot_overhead = utr_overhead + papi_overhead;
  if (dopr_preamble) {
    fprintf (fp, "If overhead stats are printed, roughly half the estimated number is\n"
             "embedded in the wallclock stats for each timer.\n"
             "Print method was %s.\n", methodstr (method));
#ifdef ENABLE_PMPI
    fprintf (fp, "If a AVG_MPI_BYTES field is present, it is an estimate of the per-call "
             "average number of bytes handled by that process.\n"
             "If timers beginning with sync_ are present, it means MPI synchronization "
             "was turned on.\n");
#endif
    fprintf (fp, "If a \'%%_of\' field is present, it is w.r.t. the first timer for thread 0.\n"
             "If a \'e6_per_sec\' field is present, it is in millions of PAPI counts per sec.\n\n"
             "A '*' in column 1 below means the timer had multiple parents, though the\n"
             "values printed are for all calls.\n"
             "Further down the listing may be more detailed information about multiple\n"
             "parents. Look for 'Multiple parent info'\n\n");
  }

  sum = (float *) GPTLallocate (nthreads * sizeof (float));
  
  for (t = 0; t < nthreads; ++t) {
    /*
    ** Construct tree for printing timers in parent/child form. get_max_depth() must be called 
    ** AFTER construct_tree() because it relies on the per-parent children arrays being complete.
    */
    if (construct_tree (timers[t], method) != 0)
      printf ("GPTLpr_file: failure from construct_tree: output will be incomplete\n");
    max_depth[t] = get_max_depth (timers[t], 0);

    if (t > 0)
      fprintf (fp, "\n");
    fprintf (fp, "Stats for thread %d:\n", t);

    for (n = 0; n < max_depth[t]+1; ++n)    /* +1 to always indent timer name */
      fprintf (fp, "  ");
    for (n = 0; n < max_name_len[t]; ++n) /* longest timer name */
      fprintf (fp, " ");
    fprintf (fp, "Called  Recurse ");

    /* Print strings for enabled timer types */

    if (cpustats.enabled)
      fprintf (fp, "%s", cpustats.str);
    if (wallstats.enabled) {
      fprintf (fp, "%s", wallstats.str);
      if (percent && timers[0]->next)
        fprintf (fp, "%%_of_%5.5s ", timers[0]->next->name);
      if (overheadstats.enabled)
        fprintf (fp, "%s", overheadstats.str);
    }

#ifdef ENABLE_PMPI
    fprintf (fp, "AVG_MPI_BYTES ");
#endif

#ifdef HAVE_PAPI
    GPTL_PAPIprstr (fp);
#endif

    fprintf (fp, "\n");        /* Done with titles, now print stats */

    /*
    ** Print call tree and stats via recursive routine. "-1" is flag to
    ** avoid printing dummy outermost timer, and initialize the depth.
    */
    printself_andchildren (timers[t], fp, t, -1, tot_overhead);

    /* 
    ** Sum of overhead across timers is meaningful.
    ** Factor of 2 is because there are 2 utr calls per start/stop pair.
    */
    sum[t]     = 0;
    totcount   = 0;
    for (ptr = timers[t]->next; ptr; ptr = ptr->next) {
      sum[t]     += ptr->count * 2 * tot_overhead;
      totcount   += ptr->count;
    }
    if (wallstats.enabled && overheadstats.enabled)
      fprintf (fp, "\n");
      fprintf (fp, "Overhead sum = %9.3g wallclock seconds\n", sum[t]);
    if (totcount < PRTHRESH)
      fprintf (fp, "Total calls  = %lu\n", totcount);
    else
      fprintf (fp, "Total calls  = %9.3e\n", (float) totcount);
  }

  /* Print per-name stats for all threads */
  if (dopr_threadsort && nthreads > 1) {
    fprintf (fp, "\nSame stats sorted by timer for threaded regions:\n");
    fprintf (fp, "Thd ");

    for (n = 0; n < max_name_len[0]; ++n) /* longest timer name */
      fprintf (fp, " ");

    fprintf (fp, "Called  Recurse ");

    if (cpustats.enabled)
      fprintf (fp, "%s", cpustats.str);
    if (wallstats.enabled) {
      fprintf (fp, "%s", wallstats.str);
      if (percent && timers[0]->next)
        fprintf (fp, "%%_of_%5.5s ", timers[0]->next->name);
      if (overheadstats.enabled)
        fprintf (fp, "%s", overheadstats.str);
    }

#ifdef HAVE_PAPI
    GPTL_PAPIprstr (fp);
#endif

    fprintf (fp, "\n");

    /* Start at next to skip dummy */
    for (ptr = timers[0]->next; ptr; ptr = ptr->next) {      
      /* 
      ** To print sum stats, first create a new timer then copy thread 0
      ** stats into it. then sum using "add", and finally print.
      */
      foundany = false;
      first = true;
      sumstats = *ptr;
      for (t = 1; t < nthreads; ++t) {
        found = false;
        for (tptr = timers[t]->next; tptr && ! found; tptr = tptr->next) {
          if (STRMATCH (ptr->name, tptr->name)) {

            /* Only print thread 0 when this timer found for other threads */
            if (first) {
              first = false;
              fprintf (fp, "%3.3d ", 0);
              printstats (ptr, fp, 0, 0, false, tot_overhead);
            }

            found = true;
            foundany = true;
            fprintf (fp, "%3.3d ", t);
            printstats (tptr, fp, 0, 0, false, tot_overhead);
            add (&sumstats, tptr);
          }
        }
      }

      if (foundany) {
        fprintf (fp, "SUM ");
        printstats (&sumstats, fp, 0, 0, false, tot_overhead);
        fprintf (fp, "\n");
      }
    }

    /* Repeat overhead print in loop over threads */
    if (wallstats.enabled && overheadstats.enabled) {
      osum = 0.;
      for (t = 0; t < nthreads; ++t) {
        fprintf (fp, "OVERHEAD.%3.3d (wallclock seconds) = %9.3g\n", t, sum[t]);
        osum += sum[t];
      }
      fprintf (fp, "OVERHEAD.SUM (wallclock seconds) = %9.3g\n", osum);
    }
  }

  /* Print info about timers with multiple parents */
  if (dopr_multparent) {
    for (t = 0; t < nthreads; ++t) {
      bool some_multparents = false;   /* thread has entries with multiple parents? */
      for (ptr = timers[t]->next; ptr; ptr = ptr->next) {
        if (ptr->nparent > 1) {
          some_multparents = true;
          break;
        }
      }

      if (some_multparents) {
        fprintf (fp, "\nMultiple parent info for thread %d:\n", t);
        if (dopr_preamble && t == 0) {
          fprintf (fp, "Columns are count and name for the listed child\n"
                   "Rows are each parent, with their common child being the last entry, "
                   "which is indented.\n"
                   "Count next to each parent is the number of times it called the child.\n"
                   "Count next to child is total number of times it was called by the "
                   "listed parents.\n\n");
        }

        for (ptr = timers[t]->next; ptr; ptr = ptr->next)
          if (ptr->nparent > 1)
            print_multparentinfo (fp, ptr);
      }
    }
  }

  /* Print hash table stats */
  if (dopr_collision) {
    for (t = 0; t < nthreads; t++) {
      first = true;
      totent   = 0;
      num_zero = 0;
      num_one  = 0;
      num_two  = 0;
      num_more = 0;
      most     = 0;

      for (i = 0; i < tablesize; i++) {
        nument = hashtable[t][i].nument;
        if (nument > 1) {
          totent += nument-1;
          if (first) {
            first = false;
            fprintf (fp, "\nthread %d had some hash collisions:\n", t);
          }
          fprintf (fp, "hashtable[%d][%d] had %d entries:", t, i, nument);
          for (ii = 0; ii < nument; ii++)
            fprintf (fp, " %s", hashtable[t][i].entries[ii]->name);
          fprintf (fp, "\n");
        }
        switch (nument) {
        case 0:
          ++num_zero;
          break;
        case 1:
          ++num_one;
          break;
        case 2:
          ++num_two;
          break;
        default:
          ++num_more;
          break;
        }
        most = MAX (most, nument);
      }

      if (totent > 0) {
        fprintf (fp, "Total collisions thread %d = %d\n", t, totent);
        fprintf (fp, "Entry information:\n");
        fprintf (fp, "num_zero = %d num_one = %d num_two = %d num_more = %d\n",
                 num_zero, num_one, num_two, num_more);
        fprintf (fp, "Most = %d\n", most);
      }
    }
    fprintf (fp, "Size of hash table was %d\n", tablesize);

  }

  /* Stats on GPTL memory usage */
  pchmem    = 0.;
  regionmem = 0.;
  papimem   = 0.;
  hashmem   = (float) sizeof (Hashentry) * tablesize * maxthreads;  /* fixed size of table */
  for (t = 0; t < nthreads; t++) {
    numtimers = 0;
    for (ptr = timers[t]->next; ptr; ptr = ptr->next) {
      ++numtimers;
      pchmem  += (float) sizeof (Timer *) * (ptr->nchildren + ptr->nparent);
    }
    hashmem   += (float) numtimers * sizeof (Timer *);
    regionmem += (float) numtimers * sizeof (Timer);
#ifdef HAVE_PAPI
    papimem += (float) numtimers * sizeof (Papistats);
#endif
  }

  totmem = hashmem + regionmem + pchmem;
  fprintf (fp, "\n");
  fprintf (fp, "Total GPTL memory usage = %g KB\n", totmem*.001);
  fprintf (fp, "Components:\n");
  fprintf (fp, "Hashmem                 = %g KB\n" 
               "Regionmem               = %g KB (papimem portion = %g KB)\n"
               "Parent/child arrays     = %g KB\n",
           hashmem*.001, regionmem*.001, papimem*.001, pchmem*.001);

  print_threadmapping (fp);
  free (sum);

  if (fp != stderr && fclose (fp) != 0)
    fprintf (stderr, "Attempt to close %s failed\n", outfile);

  pr_has_been_called = true;
  return 0;
}

/* 
** construct_tree: Build the parent->children tree starting with knowledge of
**                 parent list for each child.
**
** Input arguments:
**   timerst: Linked list of timers
**   method:  method to be used to define the links
**
** Return value: 0 (success) or GPTLerror (failure)
*/
int construct_tree (Timer *timerst, Method method)
{
  Timer *ptr;       /* loop through linked list */
  Timer *pptr = 0;  /* parent (init to NULL to avoid compiler warning) */
  int nparent;      /* number of parents */
  int maxcount;     /* max calls by a single parent */
  int n;            /* loop over nparent */
  static const char *thisfunc = "construct_tree";

  /*
  ** Walk the linked list to build the parent-child tree, using whichever
  ** mechanism is in place. newchild() will prevent loops.
  */
  for (ptr = timerst; ptr; ptr = ptr->next) {
    switch (method) {
    case GPTLfirst_parent:
      if (ptr->nparent > 0) {
        pptr = ptr->parent[0];
        if (newchild (pptr, ptr) != 0);
      }
      break;
    case GPTLlast_parent:
      if (ptr->nparent > 0) {
        nparent = ptr->nparent;
        pptr = ptr->parent[nparent-1];
        if (newchild (pptr, ptr) != 0);
      }
      break;
    case GPTLmost_frequent:
      maxcount = 0;
      for (n = 0; n < ptr->nparent; ++n) {
        if (ptr->parent_count[n] > maxcount) {
          pptr = ptr->parent[n];
          maxcount = ptr->parent_count[n];
        }
      }
      if (maxcount > 0) {   /* not an orphan */
        if (newchild (pptr, ptr) != 0);
      }
      break;
    case GPTLfull_tree:
      for (n = 0; n < ptr->nparent; ++n) {
        pptr = ptr->parent[n];
        if (newchild (pptr, ptr) != 0);
      }
      break;
    default:
      return GPTLerror ("GPTL: %s: method %d is not known\n", thisfunc, method);
    }
  }
  return 0;
}

/* 
** methodstr: Return a pointer to a string which represents the method
**
** Input arguments:
**   method: method type
*/
static char *methodstr (Method method)
{
  if (method == GPTLfirst_parent)
    return "first_parent";
  else if (method == GPTLlast_parent)
    return "last_parent";
  else if (method == GPTLmost_frequent)
    return "most_frequent";
  else if (method == GPTLfull_tree)
    return "full_tree";
  else
    return "Unknown";
}

/* 
** newchild: Add an entry to the children list of parent. Use function
**   is_descendant() to prevent infinite loops. 
**
** Input arguments:
**   parent: parent node
**   child:  child to be added
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static int newchild (Timer *parent, Timer *child)
{
  int nchildren;     /* number of children (temporary) */
  Timer **chptr;     /* array of pointers to children */
  static const char *thisfunc = "newchild";

  if (parent == child)
    return GPTLerror ("%s: child %s can't be a parent of itself\n", thisfunc, child->name);

  /*
  ** To guarantee no loops, ensure that proposed parent isn't already a descendant of 
  ** proposed child
  */
  if (is_descendant (child, parent)) {
    return GPTLerror ("GPTL: %s: loop detected: NOT adding %s to descendant list of %s. "
                      "Proposed parent is in child's descendant path.\n",
                      thisfunc, child->name, parent->name);
  }

  /* Safe to add the child to the parent's list of children */
  ++parent->nchildren;
  nchildren = parent->nchildren;
  chptr = (Timer **) realloc (parent->children, nchildren * sizeof (Timer *));
  if ( ! chptr)
    return GPTLerror ("%s: realloc error\n", thisfunc);
  parent->children = chptr;
  parent->children[nchildren-1] = child;

  return 0;
}

/* 
** get_max_depth: Determine the maximum call tree depth by traversing the
**   tree recursively
**
** Input arguments:
**   ptr:        Starting timer
**   startdepth: current depth when function invoked 
**
** Return value: maximum depth
*/
static int get_max_depth (const Timer *ptr, const int startdepth)
{
  int maxdepth = startdepth;
  int depth;
  int n;

  for (n = 0; n < ptr->nchildren; ++n)
    if ((depth = get_max_depth (ptr->children[n], startdepth+1)) > maxdepth)
      maxdepth = depth;

  return maxdepth;
}

/* 
** is_descendant: Determine whether node2 is in the descendant list for
**   node1
**
** Input arguments:
**   node1: starting node for recursive search
**   node2: node to be searched for
**
** Return value: true or false
*/
static int is_descendant (const Timer *node1, const Timer *node2)
{
  int n;

  /* Breadth before depth for efficiency */
  for (n = 0; n < node1->nchildren; ++n)
    if (node1->children[n] == node2)
      return 1;

  for (n = 0; n < node1->nchildren; ++n)
    if (is_descendant (node1->children[n], node2))
      return 1;

  return 0;
}

/* 
** printstats: print a single timer
**
** Input arguments:
**   timer:        timer for which to print stats
**   fp:           file descriptor to write to
**   t:            thread number
**   depth:        depth to indent timer
**   doindent:     whether indenting will be done
**   tot_overhead: underlying timing routine overhead
*/
static void printstats (const Timer *timer,
                        FILE *fp,
                        const int t,
                        const int depth,
                        const bool doindent,
                        const double tot_overhead)
{
  int i;               /* index */
  int indent;          /* index for indenting */
  int extraspace;      /* for padding to length of longest name */
  float fusr;          /* user time as float */
  float fsys;          /* system time as float */
  float usrsys;        /* usr + sys */
  float elapse;        /* elapsed time */
  float wallmax;       /* max wall time */
  float wallmin;       /* min wall time */
  float ratio;         /* percentage calc */

  if (timer->onflg && verbose)
    fprintf (stderr, "printstats: timer %s had not been turned off\n", timer->name);

  /* Flag regions having multiple parents with a "*" in column 1 */
  if (doindent) {
    if (timer->nparent > 1)
      fprintf (fp, "* ");
    else
      fprintf (fp, "  ");

    /* Indent to depth of this timer */
    for (indent = 0; indent < depth; ++indent)
      fprintf (fp, "  ");
  }

  fprintf (fp, "%s", timer->name);

  /* Pad to length of longest name */
  extraspace = max_name_len[t] - strlen (timer->name);
  for (i = 0; i < extraspace; ++i)
    fprintf (fp, " ");

  /* Pad to max indent level */
  if (doindent)
    for (indent = depth; indent < max_depth[t]; ++indent)
      fprintf (fp, "  ");

  if (timer->count < PRTHRESH) {
    if (timer->nrecurse > 0)
      fprintf (fp, "%8lu %6lu ", timer->count, timer->nrecurse);
    else
      fprintf (fp, "%8lu    -   ", timer->count);
  } else {
    if (timer->nrecurse > 0)
      fprintf (fp, "%8.1e %6.0e ", (float) timer->count, (float) timer->nrecurse);
    else
      fprintf (fp, "%8.1e    -   ", (float) timer->count);
  }

  if (cpustats.enabled) {
    fusr = timer->cpu.accum_utime / (float) ticks_per_sec;
    fsys = timer->cpu.accum_stime / (float) ticks_per_sec;
    usrsys = fusr + fsys;
    fprintf (fp, "%9.3f %9.3f %9.3f ", fusr, fsys, usrsys);
  }

  if (wallstats.enabled) {
    elapse = timer->wall.accum;
    wallmax = timer->wall.max;
    wallmin = timer->wall.min;

    if (elapse < 0.01)
      fprintf (fp, "%9.2e ", elapse);
    else
      fprintf (fp, "%9.3f ", elapse);

    if (wallmax < 0.01)
      fprintf (fp, "%9.2e ", wallmax);
    else
      fprintf (fp, "%9.3f ", wallmax);

    if (wallmin < 0.01)
      fprintf (fp, "%9.2e ", wallmin);
    else
      fprintf (fp, "%9.3f ", wallmin);

    if (percent && timers[0]->next) {
      ratio = 0.;
      if (timers[0]->next->wall.accum > 0.)
        ratio = (timer->wall.accum * 100.) / timers[0]->next->wall.accum;
      fprintf (fp, " %9.2f ", ratio);
    }

    /* Factor of 2 is because there are 2 utr calls per start/stop pair */
    if (overheadstats.enabled) {
      fprintf (fp, "%13.3f ", timer->count * 2 * tot_overhead);
    }
  }

#ifdef ENABLE_PMPI
  if (timer->nbytes == 0.)
    fprintf (fp, "      -       ");
  else
    fprintf (fp, "%13.3e ", timer->nbytes / timer->count);
#endif
  
#ifdef HAVE_PAPI
  GPTL_PAPIpr (fp, &timer->aux, t, timer->count, timer->wall.accum);
#endif

  fprintf (fp, "\n");
}

/* 
** print_multparentinfo: 
**
** Input arguments:
** Input/output arguments:
*/
void print_multparentinfo (FILE *fp, 
                           Timer *ptr)
{
  int n;

  if (ptr->norphan > 0) {
    if (ptr->norphan < PRTHRESH)
      fprintf (fp, "%8u %-32s\n", ptr->norphan, "ORPHAN");
    else
      fprintf (fp, "%8.1e %-32s\n", (float) ptr->norphan, "ORPHAN");
  }

  for (n = 0; n < ptr->nparent; ++n) {
    if (ptr->parent_count[n] < PRTHRESH)
      fprintf (fp, "%8d %-32s\n", ptr->parent_count[n], ptr->parent[n]->name);
    else
      fprintf (fp, "%8.1e %-32s\n", (float) ptr->parent_count[n], ptr->parent[n]->name);
  }

  if (ptr->count < PRTHRESH)
    fprintf (fp, "%8lu   %-32s\n\n", ptr->count, ptr->name);
  else
    fprintf (fp, "%8.1e   %-32s\n\n", (float) ptr->count, ptr->name);
}

/* 
** add: add the contents of tin to tout
**
** Input arguments:
**   tin:  input timer
** Input/output arguments:
**   tout: output timer summed into
*/
static void add (Timer *tout,   
                 const Timer *tin)
{
  tout->count += tin->count;

  if (wallstats.enabled) {
    tout->wall.accum += tin->wall.accum;
    
    tout->wall.max = MAX (tout->wall.max, tin->wall.max);
    tout->wall.min = MIN (tout->wall.min, tin->wall.min);
  }

  if (cpustats.enabled) {
    tout->cpu.accum_utime += tin->cpu.accum_utime;
    tout->cpu.accum_stime += tin->cpu.accum_stime;
  }
#ifdef HAVE_PAPI
  GPTL_PAPIadd (&tout->aux, &tin->aux);
#endif
}

#ifdef HAVE_MPI

/* 
** GPTLpr_summary: When MPI enabled, gather and print summary stats across threads
**                 and MPI tasks. The communication algorithm is O(log nranks) so
**                 it easily scales to thousands of ranks. Added local memory usage 
**                 is 2*(number_of_regions)*sizeof(Global) on each rank.
**
** Input arguments:
**   comm: communicator (e.g. MPI_COMM_WORLD). If zero, use MPI_COMM_WORLD
*/
int GPTLpr_summary (MPI_Comm comm)       /* communicator */
{
  int ret;             /* return code */
  int iam;             /* my rank */
  int nranks;          /* number of ranks in communicator */
  int nregions;        /* number of regions aggregated across all tasks */
  int nregions_p;      /* number of regions for a single task */
  int n, nn;           /* region index */
  int i;               /* index */
  Timer *ptr;          /* linked list pointer */
  int incr;            /* increment for tree sum */
  int twoincr;         /* 2*incr */
  int dosend;          /* logical indicating whether to send this iteration */
  int dorecv;          /* logical indicating whether to recv this iteration */
  int sendto;          /* rank to send to */
  int p;               /* rank to recv fm */
  int mnl;             /* max name length across all threads and tasks */
  MPI_Status status;   /* required by MPI_Recv */
  int extraspace;      /* for padding to length of longest name */
  int multithread;     /* flag indicates multithreaded or not for any task */
  int multithread_p;   /* recvd flag for other processor indicates multithreaded or not */
  Global *global;      /* stats to be printed accumulated across tasks */
  Global *global_p;    /* stats to be printed for a single task */
  Global *sptr;        /* realloc intermediate */
  float delta;         /* from Chan, et. al. */
  float sigma;         /* st. dev. */
  unsigned int tsksum; /* part of Chan, et. al. equation */
  static const int tag = 98789;                    /* tag for MPI message */
  static const char *outfile = "timing.summary";   /* file to write to */
  static const int nbytes = sizeof (Global);       /* number of bytes to be sent/recvd */
  static const char *thisfunc = "GPTLpr_summary";  /* this function */
  FILE *fp = 0;        /* file handle to write to */
#ifdef HAVE_PAPI
  int e;               /* event index */
#endif

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize() has not been called\n", thisfunc);

  if (((int) comm) == 0)
    comm = MPI_COMM_WORLD;

  if ((ret = MPI_Comm_rank (comm, &iam)) != MPI_SUCCESS)
    return GPTLerror ("%s: Bad return from MPI_Comm_rank=%d\n", thisfunc, ret);

  if ((ret = MPI_Comm_size (comm, &nranks)) != MPI_SUCCESS)
    return GPTLerror ("%s rank %d: Bad return from MPI_Comm_size=%d\n", thisfunc, iam, ret);

  /* Examine only thread 0 regions */
  ret = GPTLget_nregions (0, &nregions);
  global = (Global *) GPTLallocate (nregions * sizeof (Global));

  /*
  ** Gather per-thread stats based on thread 0 list.
  ** Also discover length of longest region name for formatting
  */
  n = 0;
  mnl = 0;
  for (ptr = timers[0]->next; ptr; ptr = ptr->next) {
    get_threadstats (iam, ptr->name, &global[n]);
    mnl = MAX (strlen (ptr->name), mnl);

    /* Initialize for calculating mean, st. dev. */
    global[n].mean   = global[n].wallmax;
    global[n].m2     = 0.;
    global[n].tottsk = 1;
    ++n;
  }
  if (n != nregions)
    return GPTLerror ("%s rank %d: Bad logic caused n=%d and nregions=%d\n", thisfunc, iam, n, nregions);
  
  multithread = (nthreads > 1);

  /*
  ** If all ranks participate in a region, could use MPI_Reduce to get mean and variance.
  ** But we can't assume that, so instead code the parallel algorithm by hand. 
  ** Log(ntask) algorithm to gather results to a single task is Jim Rosinski's concoction.
  ** One-pass algorithm for gathering mean and standard deviation comes from Chan et. al.
  ** (1979) described in: http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  ** Discovered by googling for "one pass standard deviation" which found the Wikipedia
  ** page pointing to the Chan et. al. work. I'm not enough of a statistical whiz to
  ** be able to map the simple 3-line algorithm in the Wikipedia page (see "Parallel 
  ** algorithm") to anything in the Chan et. al. work, but it does work.
  */
  for (incr = 1; incr < nranks; incr = twoincr) {
    twoincr = 2*incr;
    sendto = iam - incr;
    p = iam + incr;      /* could rename p as recvfm */

    /* 
    ** The && part of the next 2 stmts prevents sending to or receiving from
    ** outside communicator bounds when nranks is not a power of 2
    */
    dorecv = ((iam + twoincr) % twoincr == 0) && (p < nranks);
    dosend = ((iam + incr) % twoincr == 0) && (sendto > -1);
    if (dosend) {
      if (dorecv)
        printf ("WARNING: iam=%d: dosend and dorecv both true: possible hang?\n", iam);

      if ((ret = MPI_Send (&nregions, 1, MPI_INT, sendto, tag, comm)) != MPI_SUCCESS)
        return GPTLerror ("%s rank %d: Bad return from MPI_Send=%d\n", thisfunc, iam, ret);
      if ((ret = MPI_Send (&multithread, 1, MPI_INT, sendto, tag, comm)) != MPI_SUCCESS)
        return GPTLerror ("%s rank %d: Bad return from MPI_Send=%d\n", thisfunc, iam, ret);
      if ((ret = MPI_Send (global, nbytes*nregions, MPI_BYTE, sendto, tag, comm)) != MPI_SUCCESS)
        return GPTLerror ("%s rank %d: Bad return from MPI_Send=%d\n", thisfunc, iam, ret);
    }

    if (dorecv) {
      if (dosend)
        printf ("WARNING: iam=%d: dosend and dorecv both true: possible hang?\n", iam);

      if ((ret = MPI_Recv (&nregions_p, 1, MPI_INT, p, tag, comm, &status)) != MPI_SUCCESS)
        return GPTLerror ("%s rank %d: Bad return from MPI_Recv=%d\n", thisfunc, iam, ret);
      if ((ret = MPI_Recv (&multithread_p, 1, MPI_INT, p, tag, comm, &status)) != MPI_SUCCESS)
        return GPTLerror ("%s rank %d: Bad return from MPI_Recv=%d\n", thisfunc, iam, ret);
      if (multithread_p)
        multithread = true;

      global_p = (Global *) GPTLallocate (nregions_p * sizeof (Global));
      ret = MPI_Recv (global_p, nbytes*nregions_p, MPI_BYTE, p, tag, comm, &status);
      if (ret != MPI_SUCCESS)
        return GPTLerror ("%s rank %d: Bad return from MPI_Recv=%d\n", thisfunc, iam, ret);
      
      /* Merge stats for task p with our current stats */
      for (n = 0; n < nregions_p; ++n) {
        for (nn = 0; nn < nregions; ++nn) {
          if (STRMATCH (global_p[n].name, global[nn].name)) {
            break;
          }
        }

        if (nn == nregions) {  /* new region: reallocate and copy stats */
          ++nregions;
          sptr = realloc (global, nregions * sizeof (Global));
          if ( ! sptr)
            return GPTLerror ("%s: realloc error", thisfunc);
          global = sptr;
          /* IMPORTANT: structure copy only works because it contains NO pointers (only arrays) */
          global[nn] = global_p[n];
          mnl = MAX (strlen (global[nn].name), mnl);

        } else {               /* adjust stats for region */

          global[nn].totcalls += global_p[n].totcalls; /* count is cumulative */
          if (global_p[n].wallmax > global[nn].wallmax) {
            global[nn].wallmax   = global_p[n].wallmax;
            global[nn].wallmax_p = global_p[n].wallmax_p;
            global[nn].wallmax_t = global_p[n].wallmax_t;
          }
          if (global_p[n].wallmin < global[nn].wallmin) {
            global[nn].wallmin   = global_p[n].wallmin;
            global[nn].wallmin_p = global_p[n].wallmin_p;
            global[nn].wallmin_t = global_p[n].wallmin_t;
          }

          /* Mean, variance calcs. Cast to float avoids possible integer overflow */
          tsksum = global_p[n].tottsk + global[nn].tottsk;
          delta  = global_p[n].mean   - global[nn].mean;
          global[nn].mean += (delta * global_p[n].tottsk) / tsksum;
          global[nn].m2   += global_p[n].m2 + 
            delta * delta * ((float) global_p[n].tottsk * global[nn].tottsk) / tsksum;
          global[nn].tottsk = tsksum;

#ifdef HAVE_PAPI
          for (e = 0; e < nevents; ++e) {
            if (global_p[n].papimax[e] > global[nn].papimax[e]) {
              global[nn].papimax[e]   = global_p[n].papimax[e];
              global[nn].papimax_p[e] = p;
              global[nn].papimax_t[e] = global_p[n].papimax_t[e];
            }
            if (global_p[n].papimin[e] < global[nn].papimin[e]) {
              global[nn].papimin[e]   = global_p[n].papimin[e];
              global[nn].papimin_p[e] = p;
              global[nn].papimin_t[e] = global_p[n].papimin_t[e];
            }
          }
#endif
        }
      }
      free (global_p); /* done with received data this iteration */
    }
  }

  if (iam == 0) {
    if ( ! (fp = fopen (outfile, "w")))
      fp = stderr;

    /* Print heading */
    fprintf (fp, "Total ranks in communicator=%d\n", nranks);
    fprintf (fp, "nthreads on rank 0=%d\n", nthreads);
    fprintf (fp, "'N' used for mean, std. dev. calcs.: 'ncalls'/'nthreads'\n");
    fprintf (fp, "'ncalls': number of times the region was invoked across tasks and threads.\n");
    fprintf (fp, "'nranks': number of ranks which invoked the region.\n");
    fprintf (fp, "mean, std. dev: computed using per-rank max time across all threads on each rank\n");
    fprintf (fp, "wallmax and wallmin: max, min time across tasks and threads.\n");

    fprintf (fp, "\nname");
    extraspace = mnl - strlen ("name");
    for (n = 0; n < extraspace; ++n)
      fprintf (fp, " ");
    fprintf (fp, "   ncalls nranks mean_time   std_dev   wallmax (rank  ");
    if (multithread)
      fprintf (fp, "thread");
    fprintf (fp, ")   wallmin (rank  ");
    if (multithread)
      fprintf (fp, "thread");
    fprintf (fp, ")");

#ifdef HAVE_PAPI
    for (e = 0; e < nevents; ++e) {
      fprintf (fp, " %8.8smax (rank  ", eventlist[e].str8);
      if (multithread)
        fprintf (fp, "thread");
      fprintf (fp, ")");

      fprintf (fp, " %8.8smin (rank  ", eventlist[e].str8);
      if (multithread)
        fprintf (fp, "thread");
      fprintf (fp, ")");
    }
#endif
    fprintf (fp, "\n");

    /* Loop over regions and print summarized timing stats */
    for (n = 0; n < nregions; ++n) {
      fprintf (fp, "%s", global[n].name);
      extraspace = mnl - strlen (global[n].name);

      for (i = 0; i < extraspace; ++i)
        fprintf (fp, " ");

      if (global[n].tottsk > 1)
        sigma = sqrt ((double) global[n].m2 / (global[n].tottsk - 1));
      else
        sigma = 0.;

      if (multithread) {  /* Threads and tasks */
        if (global[n].totcalls < PRTHRESH) {
          fprintf (fp, " %8lu %6u %9.3f %9.3f %9.3f (%6d %5d) %9.3f (%6d %5d)", 
                   global[n].totcalls, global[n].tottsk, global[n].mean, sigma, 
                   global[n].wallmax, global[n].wallmax_p, global[n].wallmax_t, 
                   global[n].wallmin, global[n].wallmin_p, global[n].wallmin_t);
        } else {
          fprintf (fp, " %8.1e %6u %9.3f %9.3f %9.3f (%6d %5d) %9.3f (%6d %5d)", 
                   (float) global[n].totcalls, global[n].tottsk, global[n].mean, sigma, 
                   global[n].wallmax, global[n].wallmax_p, global[n].wallmax_t, 
                   global[n].wallmin, global[n].wallmin_p, global[n].wallmin_t);
        }
      } else {  /* No threads */
        if (global[n].totcalls < PRTHRESH) {
          fprintf (fp, " %8lu %6u %9.3f %9.3f %9.3f (%6d) %9.3f (%6d)", 
                   global[n].totcalls, global[n].tottsk, global[n].mean, sigma, 
                   global[n].wallmax, global[n].wallmax_p, 
                   global[n].wallmin, global[n].wallmin_p);
        } else {
          fprintf (fp, " %8.1e %6u %9.3f %9.3f %9.3f (%6d) %9.3f (%6d)", 
                   (float) global[n].totcalls, global[n].tottsk, global[n].mean, sigma, 
                   global[n].wallmax, global[n].wallmax_p, 
                   global[n].wallmin, global[n].wallmin_p);
        }
      }

#ifdef HAVE_PAPI
      for (e = 0; e < nevents; ++e) {
        if (multithread)
          fprintf (fp, " %8.2e    (%6d %5d)", 
                   global[n].papimax[e], global[n].papimax_p[e], 
                   global[n].papimax_t[e]);
        else
          fprintf (fp, " %8.2e    (%6d)", 
                   global[n].papimax[e], global[n].papimax_p[e]);

        if (multithread)
          fprintf (fp, " %8.2e    (%6d %5d)", 
                   global[n].papimin[e], global[n].papimin_p[e], 
                   global[n].papimin_t[e]);
        else
          fprintf (fp, " %8.2e    (%6d)", 
                   global[n].papimin[e], global[n].papimin_p[e]);
      }
#endif
      fprintf (fp, "\n");
    }
    if (fp != stderr && fclose (fp) != 0)
      fprintf (stderr, "Attempt to close %s failed\n", outfile);
  }
  free (global);
  return 0;
}

/* 
** GPTLbarrier: When MPI enabled, set and time an MPI barrier
**
** Input arguments:
**   comm: commuicator (e.g. MPI_COMM_WORLD). If zero, use MPI_COMM_WORLD
**   name: region name
**
** Return value: 0 (success)
*/
int GPTLbarrier (MPI_Comm comm, const char *name)
{
  int ret;
  static const char *thisfunc = "GPTLbarrier";

  ret = GPTLstart (name);
  if ((ret = MPI_Barrier (comm)) != MPI_SUCCESS)
    return GPTLerror ("%s: Bad return from MPI_Barrier=%d", thisfunc, ret);
  ret = GPTLstop (name);
  return 0;
}

#else

/* No MPI. Mimic MPI version but for only one rank */

int GPTLpr_summary ()
{
  static const char *outfile = "timing.summary";   /* file to write to */
  FILE *fp = 0;        /* file handle */
  int ret;
  int multithread;     /* flag indicates multithreaded or not */
  int extraspace;      /* for padding to length of longest name */
  int n;
#ifdef HAVE_PAPI
  int e;               /* event index */
#endif
  Global global;       /* stats to be printed */
  Timer *ptr;
  static const char *thisfunc = "GPTLpr_summary";  /* this function */

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize() has not been called\n", thisfunc);

  multithread = (nthreads > 1);

  if ( ! (fp = fopen (outfile, "w")))
    fp = stderr;

  /* Print heading */
  fprintf (fp, "GPTLpr_summary: GPTL was built W/O MPI\n");
  fprintf (fp, "CAUTION: Calling with multiple MPI tasks will not produce the behavior you want.\n");
  fprintf (fp, "This is because all invoking tasks will write to the same file in a race condition.\n");
  fprintf (fp, "nthreads=%d\n", nthreads);
  fprintf (fp, "'ncalls': number of times the region was invoked across threads.\n");

  fprintf (fp, "\nname");
  extraspace = max_name_len[0] - strlen ("name");
  for (n = 0; n < extraspace; ++n)
    fprintf (fp, " ");

  if (multithread)
    fprintf (fp, "   ncalls   wallmax (thred)   wallmin (thred)");
  else
    fprintf (fp, "   ncalls   walltim");

#ifdef HAVE_PAPI
  for (e = 0; e < nevents; ++e) {
    if (multithread)
      fprintf (fp, " %8.8smax (thred) %8.8smin (thred)", eventlist[e].str8, eventlist[e].str8);
    else
      fprintf (fp, " %8.8s", eventlist[e].str8);
  }
#endif
  fprintf (fp, "\n");

  for (ptr = timers[0]->next; ptr; ptr = ptr->next) {
    get_threadstats (0, ptr->name, &global);
    extraspace = max_name_len[0] - strlen (global.name);

    fprintf (fp, "%s", global.name);
    for (n = 0; n < extraspace; ++n)
      fprintf (fp, " ");
    if (multithread) {
      if (global.totcalls < PRTHRESH) {
	fprintf (fp, " %8lu %9.3f (%5d) %9.3f (%5d)", 
		 global.totcalls, global.wallmax, global.wallmax_t, global.wallmin, global.wallmin_t);
      } else {
	fprintf (fp, " %8.1e %9.3f (%5d) %9.3f (%5d)", 
		 (float) global.totcalls, global.wallmax, global.wallmax_t, global.wallmin, global.wallmin_t);
      }
    } else {  /* No threads */
      if (global.totcalls < PRTHRESH) {
	fprintf (fp, " %8lu %9.3f",          global.totcalls, global.wallmax);
      } else {
	fprintf (fp, " %8.1e %9.3f", (float) global.totcalls, global.wallmax);
      }
    }
#ifdef HAVE_PAPI
    for (e = 0; e < nevents; ++e) {
      if (multithread)
	fprintf (fp, " %8.2e    (%5d)", global.papimax[e], global.papimax_t[e]);
      else
	fprintf (fp, " %8.2e",          global.papimax[e]);

      if (multithread)
	fprintf (fp, " %8.2e    (%5d)", global.papimin[e], global.papimin_t[e]);
    }
#endif
    fprintf (fp, "\n");
  }
  if (fp != stderr && fclose (fp) != 0)
    fprintf (stderr, "Attempt to close %s failed\n", outfile);

  return 0;
}

#endif    /* false branch of ifdef HAVE_MPI */

/* 
** get_threadstats: gather stats for timer "name" over all threads
**
** Input arguments:
**   iam: my rank
**   name:  timer name
**   global: pointer to struct containing stats
** Output arguments:
**   global: max/min stats over all threads
*/
void get_threadstats (int iam,
                      const char *name, 
                      Global *global)
{
  int t;                /* thread index */
  unsigned int indx;    /* returned from getentry() */
  Timer *ptr;           /* timer */

  /* This memset fortuitiously initializes the process values to master (0) */
  memset (global, 0, sizeof (Global));
  strcpy (global->name, name);

  for (t = 0; t < nthreads; ++t) {
    if ((ptr = getentry (hashtable[t], name, &indx))) {
      global->totcalls += ptr->count;

      if (ptr->wall.accum > global->wallmax) {
        global->wallmax   = ptr->wall.accum;
        global->wallmax_p = iam;
        global->wallmax_t = t;
      }

      /* global->wallmin = 0 for first thread */
      if (ptr->wall.accum < global->wallmin || global->wallmin == 0.) {
        global->wallmin   = ptr->wall.accum;
        global->wallmin_p = iam;
        global->wallmin_t = t;
      }
#ifdef HAVE_PAPI
      int e;
      for (e = 0; e < nevents; ++e) {
        double value;
        if (GPTL_PAPIget_eventvalue (eventlist[e].namestr, &ptr->aux, &value) != 0) {
          fprintf (stderr, "Bad return from GPTL_PAPIget_eventvalue\n");
          return;
        }
        if (value > global->papimax[e]) {
          global->papimax[e]   = value;
          global->papimax_p[e] = iam;
          global->papimax_t[e] = t;
        }
        
	/* First thread value in global is zero */
        if (value < global->papimin[e] || global->papimin[e] == 0.) {
          global->papimin[e]   = value;
          global->papimin_p[e] = iam;
          global->papimin_t[e] = t;
        }
      }
#endif
    }
  }
}

/*
** get_cpustamp: Invoke the proper system timer and return stats.
**
** Output arguments:
**   usr: user time
**   sys: system time
**
** Return value: 0 (success)
*/
static inline int get_cpustamp (long *usr, long *sys)
{
#ifdef HAVE_TIMES
  struct tms buf;

  (void) times (&buf);
  *usr = buf.tms_utime;
  *sys = buf.tms_stime;
  return 0;
#else
  return GPTLerror ("GPTL: get_cpustamp: times() not available\n");
#endif
}

/*
** GPTLquery: return current status info about a timer. If certain stats are not 
** enabled, they should just have zeros in them. If PAPI is not enabled, input
** counter info is ignored.
** 
** Input args:
**   name:        timer name
**   maxcounters: max number of PAPI counters to get info for
**   t:           thread number (if < 0, the request is for the current thread)
**
** Output args:
**   count:            number of times this timer was called
**   onflg:            whether timer is currently on
**   wallclock:        accumulated wallclock time
**   usr:              accumulated user CPU time
**   sys:              accumulated system CPU time
**   papicounters_out: accumulated PAPI counters
*/
int GPTLquery (const char *name, 
               int t,
               int *count,
               int *onflg,
               double *wallclock,
               double *dusr,
               double *dsys,
               long long *papicounters_out,
               const int maxcounters)
{
  Timer *ptr;                /* linked list pointer */
  unsigned int indx;         /* linked list index returned from getentry (unused) */
  static const char *thisfunc = "GPTLquery";
  
  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);
  
  /* If t is < 0, assume the request is for the current thread */
  if (t < 0) {
    if ((t = get_thread_num ()) < 0)
      return GPTLerror ("%s: get_thread_num failure\n", thisfunc);
  } else {
    if (t >= maxthreads)
      return GPTLerror ("%s: requested thread %d is too big\n", thisfunc, t);
  }
  
  ptr = getentry (hashtable[t], name, &indx);
  if ( !ptr)
    return GPTLerror ("%s: requested timer %s does not have a name hash\n", thisfunc, name);

  *onflg     = ptr->onflg;
  *count     = ptr->count;
  *wallclock = ptr->wall.accum;
  *dusr      = ptr->cpu.accum_utime / (double) ticks_per_sec;
  *dsys      = ptr->cpu.accum_stime / (double) ticks_per_sec;
#ifdef HAVE_PAPI
  GPTL_PAPIquery (&ptr->aux, papicounters_out, maxcounters);
#endif
  return 0;
}

/*
** GPTLquerycounters: return current PAPI counters for a timer.
** THIS ROUTINE ID DEPRECATED. USE GPTLget_eventvalue() instead
** 
** Input args:
**   name: timer name
**   t:    thread number (if < 0, the request is for the current thread)
**
** Output args:
**   papicounters_out: accumulated PAPI counters
*/
int GPTLquerycounters (const char *name, 
                       int t,
                       long long *papicounters_out)
{
  Timer *ptr;            /* linked list pointer */
  unsigned int indx;     /* hash index returned from getentry */
  static const char *thisfunc = "GPTLquery_counters";
  
  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);
  
  /*
  ** If t is < 0, assume the request is for the current thread
  */
  
  if (t < 0) {
    if ((t = get_thread_num ()) < 0)
      return GPTLerror ("%s: get_thread_num failure\n", thisfunc);
  } else {
    if (t >= maxthreads)
      return GPTLerror ("%s: requested thread %d is too big\n", thisfunc, t);
  }
  
  ptr = getentry (hashtable[t], name, &indx);
  if ( !ptr)
    return GPTLerror ("%s: requested timer %s does not have a name hash\n", thisfunc, name);

#ifdef HAVE_PAPI
  /* MAX_AUX is the max possible number of PAPI-based events */
  GPTL_PAPIquery (&ptr->aux, papicounters_out, MAX_AUX);
#endif
  return 0;
}

/*
** GPTLget_wallclock: return wallclock accumulation for a timer.
** 
** Input args:
**   timername: timer name
**   t:         thread number (if < 0, the request is for the current thread)
**
** Output args:
**   value: current wallclock accumulation for the timer
*/
int GPTLget_wallclock (const char *timername,
                      int t,
                      double *value)
{
  void *self;          /* timer address when hash entry generated with *_instr */
  Timer *ptr;          /* linked list pointer */
  unsigned int indx;   /* hash index returned from getentry (unused) */
  static const char *thisfunc = "GPTLget_wallclock";
  
  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);

  if ( ! wallstats.enabled)
    return GPTLerror ("%s: wallstats not enabled\n", thisfunc);
  
  /* If t is < 0, assume the request is for the current thread */
  if (t < 0) {
    if ((t = get_thread_num ()) < 0)
      return GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);
  } else {
    if (t >= maxthreads)
      return GPTLerror ("%s: requested thread %d is too big\n", thisfunc, t);
  }
  
  /* 
  ** Don't know whether hashtable entry for timername was generated with 
  ** *_instr() or not, so try both possibilities
  */
  ptr = getentry (hashtable[t], timername, &indx);
  if ( !ptr) {
    if (sscanf (timername, "%lx", (unsigned long *) &self) < 1)
      return GPTLerror ("%s: requested timer %s does not exist\n", thisfunc, timername);
    ptr = getentry_instr (hashtable[t], self, &indx);
    if ( !ptr)
      return GPTLerror ("%s: requested timer %s does not exist\n", thisfunc, timername);
  }
  *value = ptr->wall.accum;
  return 0;
}

/*
** GPTLget_eventvalue: return PAPI-based event value for a timer. All values will be
**   returned as doubles, even if the event is not derived.
** 
** Input args:
**   timername: timer name
**   eventname: event name (must be currently enabled)
**   t:         thread number (if < 0, the request is for the current thread)
**
** Output args:
**   value: current value of the event for this timer
*/
int GPTLget_eventvalue (const char *timername,
                        const char *eventname,
                        int t,
                        double *value)
{
  void *self;          /* timer address when hash entry generated with *_instr */
  Timer *ptr;          /* linked list pointer */
  unsigned int indx;   /* hash index returned from getentry (unused) */
  static const char *thisfunc = "GPTLget_eventvalue";
  
  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);
  
  /* If t is < 0, assume the request is for the current thread */
  if (t < 0) {
    if ((t = get_thread_num ()) < 0)
      return GPTLerror ("%s: get_thread_num failure\n", thisfunc);
  } else {
    if (t >= maxthreads)
      return GPTLerror ("%s: requested thread %d is too big\n", thisfunc, t);
  }
  
  /* 
  ** Don't know whether hashtable entry for timername was generated with 
  ** *_instr() or not, so try both possibilities
  */
  ptr = getentry (hashtable[t], timername, &indx);
  if ( !ptr) {
    if (sscanf (timername, "%lx", (unsigned long *) &self) < 1)
      return GPTLerror ("%s: requested timer %s does not exist\n", thisfunc, timername);
    ptr = getentry_instr (hashtable[t], self, &indx);
    if ( !ptr)
      return GPTLerror ("%s: requested timer %s does not exist\n", thisfunc, timername);
  }

#ifdef HAVE_PAPI
  return GPTL_PAPIget_eventvalue (eventname, &ptr->aux, value);
#else
  return GPTLerror ("%s: PAPI not enabled\n", thisfunc); 
#endif
}

/*
** GPTLget_nregions: return number of regions (i.e. timer names) for this thread
** 
** Input args:
**   t:    thread number (if < 0, the request is for the current thread)
**
** Output args:
**   nregions: number of regions
*/
int GPTLget_nregions (int t, 
                      int *nregions)
{
  Timer *ptr;     /* walk through linked list */
  static const char *thisfunc = "GPTLget_nregions";

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);
  
  /*
  ** If t is < 0, assume the request is for the current thread
  */
  
  if (t < 0) {
    if ((t = get_thread_num ()) < 0)
      return GPTLerror ("%s: get_thread_num failure\n", thisfunc);
  } else {
    if (t >= maxthreads)
      return GPTLerror ("%s: requested thread %d is too big\n", thisfunc, t);
  }
  
  *nregions = 0;
  for (ptr = timers[t]->next; ptr; ptr = ptr->next) 
    ++*nregions;

  return 0;
}

/*
** GPTLget_regionname: return region name for this thread
** 
** Input args:
**   t:      thread number (if < 0, the request is for the current thread)
**   region: region number
**   nc:     max number of chars to put in name
**
** Output args:
**   name    region name
*/
int GPTLget_regionname (int t,      /* thread number */
                        int region, /* region number (0-based) */
                        char *name, /* output region name */
                        int nc)     /* number of chars in name (free form Fortran) */
{
  int ncpy;    /* number of characters to copy */
  int i;       /* index */
  Timer *ptr;  /* walk through linked list */
  static const char *thisfunc = "GPTLget_regionname";

  if ( ! initialized)
    return GPTLerror ("%s: GPTLinitialize has not been called\n", thisfunc);
  
  /*
  ** If t is < 0, assume the request is for the current thread
  */
  
  if (t < 0) {
    if ((t = get_thread_num ()) < 0)
      return GPTLerror ("%s: get_thread_num failure\n", thisfunc);
  } else {
    if (t >= maxthreads)
      return GPTLerror ("%s: requested thread %d is too big\n", thisfunc, t);
  }
  
  ptr = timers[t]->next;
  for (i = 0; i < region; i++) {
    if ( ! ptr)
      return GPTLerror ("%s: timer number %d does not exist in thread %d\n", thisfunc, region, t);
    ptr = ptr->next;
  }

  if (ptr) {
    ncpy = MIN (nc, strlen (ptr->name));
    strncpy (name, ptr->name, ncpy);
    
    /* Adding the \0 is only important when called from C */
    if (ncpy < nc)
      name[ncpy] = '\0';
  } else {
    return GPTLerror ("%s: timer number %d does not exist in thread %d\n", thisfunc, region, t);
  }
  return 0;
}

/*
** GPTLis_initialized: Return whether GPTL has been initialized
*/
int GPTLis_initialized (void)
{
  return (int) initialized;
}

/*
** getentry_instr: find hash table entry and return a pointer to it
**
** Input args:
**   hashtable: the hashtable (array)
**   self:      input address (from -finstrument-functions)
** Output args:
**   indx:      hashtable index
**
** Return value: pointer to the entry, or NULL if not found
*/
static inline Timer *getentry_instr (const Hashentry *hashtable, /* hash table */
                                     void *self,                 /* address */
                                     unsigned int *indx)         /* hash index */
{
  int i;
  Timer *ptr = 0;  /* return value when entry not found */

  /*
  ** Hash index is timer address modulo the table size
  ** On most machines, right-shifting the address helps because linkers often
  ** align functions on even boundaries
  */
  *indx = (((unsigned long) self) >> 4) % tablesize;
  for (i = 0; i < hashtable[*indx].nument; ++i) {
    if (hashtable[*indx].entries[i]->address == self) {
      ptr = hashtable[*indx].entries[i];
      break;
    }
  }
  return ptr;
}

/*
** getentry: find the entry in the hash table and return a pointer to it.
**
** Input args:
**   hashtable: the hashtable (array)
**   name:      string to be hashed on (specifically, summed)
** Output args:
**   indx:      hashtable index
**
** Return value: pointer to the entry, or NULL if not found
*/
static inline Timer *getentry (const Hashentry *hashtable, /* hash table */
                               const char *name,           /* name to hash */
                               unsigned int *indx)         /* hash index */
{
  int i;                      /* multiplier for hashing; loop index */
  const unsigned char *c;     /* pointer to elements of "name" */
  Timer *ptr = 0;             /* return value when entry not found */

  /* Hash value is sum of: chars times their 1-based position index, modulo tablesize */
  *indx = 0;
  c = (unsigned char *) name;
  for (i = 1; *c && i < MAX_CHARS+1; ++c, ++i) {
    *indx += (*c) * i;
  }

  *indx %= tablesize;

  /* 
  ** If nument exceeds 1 there was a hash collision and we must search
  ** linearly through an array for a match
  */
  for (i = 0; i < hashtable[*indx].nument; i++) {
    if (STRMATCH (name, hashtable[*indx].entries[i]->name)) {
      ptr = hashtable[*indx].entries[i];
      break;
    }
  }
  return ptr;
}

/*
** Add entry points for auto-instrumented codes
** Auto instrumentation flags for various compilers:
**
** gcc, pathcc, icc: -finstrument-functions
** pgcc:             -Minstrument:functions
** xlc:              -qdebug=function_trace
*/
#ifdef __cplusplus
extern "C" {
#endif

#ifdef _AIX
void __func_trace_enter (const char *function_name,
                         const char *file_name,
                         int line_number,
                         void **const user_data)
{
  char msg[MSGSIZ];

  if (dopr_memusage) {
    snprintf (msg, MSGSIZ, "begin %s", function_name);
    (void) GPTLprint_memusage (msg);
  }
  (void) GPTLstart (function_name);
}

void __func_trace_exit (const char *function_name,
                        const char *file_name,
                        int line_number,
                        void **const user_data)
{
  (void) GPTLstop (function_name);
  if (dopr_memusage) {
    snprintf (msg, MSGSIZ, "end %s", function_name);
    (void) GPTLprint_memusage (msg);
  }
}

#else

void __cyg_profile_func_enter (void *this_fn,
                               void *call_site)
{
#ifdef HAVE_BACKTRACE
  void *buffer[2];
  int nptrs;
  char **strings;
#endif
  char msg[MSGSIZ];

  if (dopr_memusage) {
#ifdef HAVE_BACKTRACE
    nptrs = backtrace (buffer, 2);
    strings = backtrace_symbols (buffer, nptrs);
    snprintf (msg, MSGSIZ, "begin %s", strings[1]);
    free (strings);
#else
    snprintf (msg, MSGSIZ, "begin %lx", (unsigned long) this_fn);
#endif
    (void) GPTLprint_memusage (msg);
  }
  (void) GPTLstart_instr (this_fn);
}

void __cyg_profile_func_exit (void *this_fn,
                              void *call_site)
{
#ifdef HAVE_BACKTRACE
  void *buffer[2];
  int nptrs;
  char **strings;
#endif
  char msg[MSGSIZ];

  (void) GPTLstop_instr (this_fn);

  if (dopr_memusage) {
#ifdef HAVE_BACKTRACE
    nptrs = backtrace (buffer, 2);
    strings = backtrace_symbols (buffer, nptrs);
    snprintf (msg, MSGSIZ, "end %s", (char *) strings[1]);
    free (strings);
#else
    snprintf (msg, MSGSIZ, "end %lx", (unsigned long) this_fn);
#endif
    (void) GPTLprint_memusage (msg);
  }
}
#endif

#ifdef __cplusplus
};
#endif

#ifdef HAVE_NANOTIME
/* Copied from PAPI library */
static inline long long nanotime (void)
{
  long long val = 0;
#ifdef BIT64
  do {
    unsigned int a, d;
    asm volatile ("rdtsc":"=a" (a), "=d" (d));
    (val) = ((long long) a) | (((long long) d) << 32);
  } while (0);
#else
  __asm__ __volatile__("rdtsc":"=A" (val): );
#endif
  return val;
}

#define LEN 4096

static float get_clockfreq ()
{
  FILE *fd = 0;
  char buf[LEN];
  int is;
  float freq = -1.;             /* clock frequency (MHz) */
  static const char *thisfunc = "get_clockfreq";
  static char *max_freq_fn = "/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq";
  static char *cpuinfo_fn = "/proc/cpuinfo";

  /* First look for max_freq, but that isn't guaranteed to exist */

  if ((fd = fopen (max_freq_fn, "r"))) {
    if (fgets (buf, LEN, fd)) {
      freq = 0.001 * (float) atof (buf);  /* Convert from KHz to MHz */
      if (verbose)
        printf ("GPTL: %s: Using max clock freq = %f for timing\n", thisfunc, freq);
      (void) fclose (fd);
      clock_source = max_freq_fn;
      return freq;
    } else {
      (void) fclose (fd);
    }
  }

  /* 
  ** Next try /proc/cpuinfo. That has the disadvantage that it may give wrong info
  ** for processors that have either idle or turbo mode
  */
  if (verbose && freq < 0.)
    printf ("GPTL: %s: CAUTION: Can't find max clock freq. Trying %s instead\n",
            thisfunc, cpuinfo_fn);

  if ( ! (fd = fopen (cpuinfo_fn, "r"))) {
    fprintf (stderr, "get_clockfreq: can't open %s\n", cpuinfo_fn);
    return -1.;
  }

  while (fgets (buf, LEN, fd)) {
    if (strncmp (buf, "cpu MHz", 7) == 0) {
      for (is = 7; buf[is] != '\0' && !isdigit (buf[is]); is++);
      if (isdigit (buf[is])) {
        freq = (float) atof (&buf[is]);
        (void) fclose (fd);
        clock_source = cpuinfo_fn;
        return freq;
      }
    }
  }

  (void) fclose (fd);
  return -1.;
}
#endif

/*
** The following are the set of underlying timing routines which may or may
** not be available. And their accompanying init routines.
** NANOTIME is currently only available on x86.
*/
static int init_nanotime ()
{
  static const char *thisfunc = "init_nanotime";
#ifdef HAVE_NANOTIME
  if ((cpumhz = get_clockfreq ()) < 0)
    return GPTLerror ("%s: Can't get clock freq\n", thisfunc);

  if (verbose)
    printf ("%s: Clock rate = %f MHz\n", thisfunc, cpumhz);

  cyc2sec = 1./(cpumhz * 1.e6);
  return 0;
#else
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

static inline double utr_nanotime ()
{
#ifdef HAVE_NANOTIME
  double timestamp;
  timestamp = nanotime () * cyc2sec;
  return timestamp;
#else
  static const char *thisfunc = "utr_nanotime";
  (void) GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
  return -1.;
#endif
}

/*
** MPI_Wtime requires MPI lib.
*/
static int init_mpiwtime ()
{
#ifdef HAVE_MPI
  return 0;
#else
  static const char *thisfunc = "init_mpiwtime";
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

static inline double utr_mpiwtime ()
{
#ifdef HAVE_MPI
  return MPI_Wtime ();
#else
  static const char *thisfunc = "utr_mpiwtime";
  (void) GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
  return -1.;
#endif
}

/*
** PAPI_get_real_usec requires PAPI lib.
*/
static int init_papitime ()
{
  static const char *thisfunc = "init_papitime";
#ifdef HAVE_PAPI
  ref_papitime = PAPI_get_real_usec ();
  if (verbose)
    printf ("%s: ref_papitime=%ld\n", thisfunc, (long) ref_papitime);
  return 0;
#else
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}
  
static inline double utr_papitime ()
{
#ifdef HAVE_PAPI
  return (PAPI_get_real_usec () - ref_papitime) * 1.e-6;
#else
  static const char *thisfunc = "utr_papitime";
  (void) GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
  return -1.;
#endif
}

/* 
** Probably need to link with -lrt for this one to work 
*/
static int init_clock_gettime ()
{
  static const char *thisfunc = "init_clock_gettime";
#ifdef HAVE_LIBRT
  struct timespec tp;
  (void) clock_gettime (CLOCK_REALTIME, &tp);
  ref_clock_gettime = tp.tv_sec;
  if (verbose)
    printf ("%s: ref_clock_gettime=%ld\n", thisfunc, (long) ref_clock_gettime);
  return 0;
#else
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

static inline double utr_clock_gettime ()
{
#ifdef HAVE_LIBRT
  struct timespec tp;
  (void) clock_gettime (CLOCK_REALTIME, &tp);
  return (tp.tv_sec - ref_clock_gettime) + 1.e-9*tp.tv_nsec;
#else
  static const char *thisfunc = "utr_clock_gettime";
  (void) GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
  return -1.;
#endif
}

/*
** High-res timer on AIX: read_real_time
*/
static int init_read_real_time ()
{
  static const char *thisfunc = "init_read_real_time";
#ifdef _AIX
  timebasestruct_t ibmtime;
  (void) read_real_time (&ibmtime, TIMEBASE_SZ);
  (void) time_base_to_time (&ibmtime, TIMEBASE_SZ);
  ref_read_real_time = ibmtime.tb_high;
  if (verbose)
    printf ("%s: ref_read_real_time=%ld\n", thisfunc, (long) ref_read_real_time);
  return 0;
#else
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

static inline double utr_read_real_time ()
{
#ifdef _AIX
  timebasestruct_t ibmtime;
  (void) read_real_time (&ibmtime, TIMEBASE_SZ);
  (void) time_base_to_time (&ibmtime, TIMEBASE_SZ);
  return (ibmtime.tb_high - ref_read_real_time) + 1.e-9*ibmtime.tb_low;
#else
  static const char *thisfunc = "utr_read_real_time";
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

/*
** Default available most places: gettimeofday
*/
static int init_gettimeofday ()
{
  static const char *thisfunc = "init_gettimeofday";
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;
  (void) gettimeofday (&tp, 0);
  ref_gettimeofday = tp.tv_sec;
  if (verbose)
    printf ("%s: ref_gettimeofday=%ld\n", thisfunc, (long) ref_gettimeofday);
  return 0;
#else
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

static inline double utr_gettimeofday ()
{
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;
  (void) gettimeofday (&tp, 0);
  return (tp.tv_sec - ref_gettimeofday) + 1.e-6*tp.tv_usec;
#else
  static const char *thisfunc = "utr_gettimeofday";
  return GPTLerror ("GPTL: %s: not enabled\n", thisfunc);
#endif
}

/*
** placebo: does nothing and returns zero always. Useful for estimating overhead costs
*/
static int init_placebo ()
{
  return 0;
}

static inline double utr_placebo ()
{
  static const double zero = 0.;
  return zero;
}

/* 
** Determine underlying timing routine overhead: call it 100 times.
*/
static double utr_getoverhead ()
{
  double val1;
  double val2;
  int i;

  val1 = (*ptr2wtimefunc)();
  for (i = 0; i < 10; ++i) {
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
    val2 = (*ptr2wtimefunc)();
  }
  return 0.01 * (val2 - val1);
}

/*
** printself_andchildren: Recurse through call tree, printing stats for self, then children
*/
static void printself_andchildren (const Timer *ptr,
                                   FILE *fp, 
                                   const int t,
                                   const int depth,
                                   const double tot_overhead)
{
  int n;

  if (depth > -1)     /* -1 flag is to avoid printing stats for dummy outer timer */
    printstats (ptr, fp, t, depth, true, tot_overhead);

  for (n = 0; n < ptr->nchildren; n++)
    printself_andchildren (ptr->children[n], fp, t, depth+1, tot_overhead);
}

#ifdef ENABLE_PMPI
/*
** GPTLgetentry: called ONLY from pmpi.c (i.e. not a public entry point). Returns a pointer to the 
**               requested timer name by calling internal function getentry()
** 
** Return value: 0 (NULL) or the return value of getentry()
*/
Timer *GPTLgetentry (const char *name)
{
  int t;                /* thread number */
  unsigned int indx;    /* returned from getentry (unused) */
  static const char *thisfunc = "GPTLgetentry";

  if ( ! initialized) {
    (void) GPTLerror ("%s: initialization was not completed\n", thisfunc);
    return 0;
  }

  if ((t = get_thread_num ()) < 0) {
    (void) GPTLerror ("%s: bad return from get_thread_num\n", thisfunc);
    return 0;
  }

  return (getentry (hashtable[t], name, &indx));
}

/*
** GPTLpr_file_has_been_called: Called ONLY from pmpi.c (i.e. not a public entry point). Return 
**                              whether GPTLpr_file has been called. MPI_Finalize wrapper needs
**                              to know whether it needs to call GPTLpr.
*/
int GPTLpr_has_been_called (void)
{
  return (int) pr_has_been_called;
}

#endif

/*************************************************************************************/

/*
** Contents of inserted threadutil.c starts here.
** Moved to gptl.c to enable inlining
*/

/*
**
** Author: Jim Rosinski
** 
** Utility functions handle thread-based GPTL needs.
*/

/**********************************************************************************/
/* 
** 3 sets of routines: OMP threading, PTHREADS, unthreaded
*/

#if ( defined THREADED_OMP )

/*
** threadinit: Allocate and initialize threadid_omp; set max number of threads
**
** Output results:
**   maxthreads: max number of threads
**
**   threadid_omp[] is allocated and initialized to -1
**
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static int threadinit (void)
{
  int t;  /* loop index */
  static const char *thisfunc = "threadinit";

  if (omp_get_thread_num () != 0)
    return GPTLerror ("OMP %s: MUST only be called by the master thread\n", thisfunc);

  /* 
  ** Allocate the threadid array which maps physical thread IDs to logical IDs 
  ** For OpenMP this will be just threadid_omp[iam] = iam;
  */
  if (threadid_omp) 
    return GPTLerror ("OMP %s: has already been called.\nMaybe mistakenly called by multiple threads?", 
                      thisfunc);

  /*
  ** maxthreads may have been set by the user, in which case use that. But if as 
  ** yet uninitialized, set to the current value of OMP_NUM_THREADS. 
  */
  if (maxthreads == -1)
    maxthreads = MAX ((1), (omp_get_max_threads ()));

  if ( ! (threadid_omp = (int *) GPTLallocate (maxthreads * sizeof (int))))
    return GPTLerror ("OMP %s: malloc failure for %d elements of threadid_omp\n", thisfunc, maxthreads);

  /*
  ** Initialize threadid array to flag values for use by get_thread_num().
  ** get_thread_num() will fill in the values on first use.
  */
  for (t = 0; t < maxthreads; ++t)
    threadid_omp[t] = -1;

#ifdef VERBOSE
  printf ("OMP %s: Set maxthreads=%d\n", thisfunc, maxthreads);
#endif
  
  return 0;
}

/*
** Threadfinalize: clean up
**
** Output results:
**   threadid_omp array is freed and array pointer nullified
*/
static void threadfinalize ()
{
  free ((void *) threadid_omp);
  threadid_omp = 0;
}

/*
** get_thread_num: Determine thread number of the calling thread
**                 Start PAPI counters if enabled and first call for this thread.
**
** Output results:
**   nthreads:     Number of threads
**   threadid_omp: Our thread id added to list on 1st call
**
** Return value: thread number (success) or GPTLerror (failure)
*/
static inline int get_thread_num (void)
{
  int t;        /* thread number */
  static const char *thisfunc = "get_thread_num";

  if ((t = omp_get_thread_num ()) >= maxthreads)
    return GPTLerror ("OMP %s: returned id=%d exceeds maxthreads=%d\n", thisfunc, t, maxthreads);

  /* If our thread number has already been set in the list, we are done */
  if (t == threadid_omp[t])
    return t;

  /* 
  ** Thread id not found. Modify threadid_omp with our ID, then start PAPI events if required.
  ** Due to the setting of threadid_omp, everything below here will only execute once per thread.
  */
  threadid_omp[t] = t;

#ifdef VERBOSE
  printf ("OMP %s: 1st call t=%d\n", thisfunc, t);
#endif

#ifdef HAVE_PAPI

  /*
  ** When HAVE_PAPI is true, if 1 or more PAPI events are enabled,
  ** create and start an event set for the new thread.
  */
  if (GPTLget_npapievents () > 0) {
#ifdef VERBOSE
    printf ("OMP %s: Starting EventSet t=%d\n", thisfunc, t);
#endif

    if (GPTLcreate_and_start_events (t) < 0)
      return GPTLerror ("GPTL: OMP %s: error from GPTLcreate_and_start_events for thread %d\n", 
			thisfunc, t);
  }
#endif

  /* nthreads = maxthreads based on setting in threadinit or user call to GPTLsetoption() */
  nthreads = maxthreads;
#ifdef VERBOSE
  printf ("GPTL: OMP %s: nthreads=%d\n", thisfunc, nthreads);
#endif

  return t;
}

static void print_threadmapping (FILE *fp)
{
  int n;

  fprintf (fp, "\n");
  fprintf (fp, "Thread mapping:\n");
  for (n = 0; n < nthreads; ++n)
    fprintf (fp, "threadid_omp[%d] = %d\n", n, threadid_omp[n]);
}

/**********************************************************************************/
/* 
** PTHREADS
*/

#elif ( defined THREADED_PTHREADS )

/*
** threadinit: Allocate threadid and initialize to -1; set max number of threads;
**             Initialize the mutex for later use; Initialize nthreads to 0
**
** Output results:
**   nthreads:   number of threads (init to zero here, increment later in get_thread_num)
**   maxthreads: max number of threads (MAX_THREADS)
**
**   threadid[] is allocated and initialized to -1
**   mutex is initialized for future use
**
** Return value: 0 (success) or GPTLerror (failure)
*/
static int threadinit (void)
{
  int t;        /* thread number */
  int ret;      /* return code */
  static const char *thisfunc = "threadinit";

  /*
  ** The following test is not rock-solid, but it's pretty close in terms of guaranteeing that 
  ** threadinit gets called by only 1 thread. Problem is, mutex hasn't yet been initialized
  ** so we can't use it.
  */
  if (nthreads == -1)
    nthreads = 0;
  else
    return GPTLerror ("GPTL: PTHREADS %s: has already been called.\n"
                      "Maybe mistakenly called by multiple threads?\n", thisfunc);

  /*
  ** Initialize the mutex required for critical regions.
  ** Previously, t_mutex = PTHREAD_MUTEX_INITIALIZER on the static declaration line was
  ** adequate to initialize the mutex. But this failed in programs that invoked
  ** GPTLfinalize() followed by GPTLinitialize().
  ** "man pthread_mutex_init" indicates that passing NULL as the second argument to 
  ** pthread_mutex_init() should appropriately initialize the mutex, assuming it was
  ** properly destroyed by a previous call to pthread_mutex_destroy();
  */
#ifdef MUTEX_API
  if ((ret = pthread_mutex_init ((pthread_mutex_t *) &t_mutex, NULL)) != 0)
    return GPTLerror ("GPTL: PTHREADS %s: mutex init failure: ret=%d\n", thisfunc, ret);
#endif
  
  /* maxthreads is either its default initialization value, or set by a user
  ** call to GPTLsetoption().
  ** Allocate the threadid array which maps physical thread IDs to logical IDs
  */
  if (threadid) 
    return GPTLerror ("GPTL: PTHREADS %s: threadid not null\n", thisfunc);
  else if ( ! (threadid = (pthread_t *) GPTLallocate (maxthreads * sizeof (pthread_t))))
    return GPTLerror ("GPTL: PTHREADS %s: malloc failure for %d elements of threadid\n", 
                      thisfunc, maxthreads);

  /*
  ** Initialize threadid array to flag values for use by get_thread_num().
  ** get_thread_num() will fill in the values on first use.
  */
  for (t = 0; t < maxthreads; ++t)
    threadid[t] = (pthread_t) -1;

#ifdef VERBOSE
  printf ("GPTL: PTHREADS %s: Set maxthreads=%d nthreads=%d\n", thisfunc, maxthreads, nthreads);
#endif

  return 0;
}

/*
** threadfinalize: Clean up
**
** Output results:
**   threadid array is freed and array pointer nullified
**   mutex is destroyed
*/
static void threadfinalize ()
{
  int ret;

#ifdef MUTEX_API
  if ((ret = pthread_mutex_destroy ((pthread_mutex_t *) &t_mutex)) != 0)
    printf ("GPTL: threadfinalize: failed attempt to destroy t_mutex: ret=%d\n", ret);
#endif
  free ((void *) threadid);
  threadid = 0;
}

/*
** get_thread_num: Determine zero-based thread number of the calling thread.
**                 Update nthreads and maxthreads if necessary.
**                 Start PAPI counters if enabled and first call for this thread.
**
** Output results:
**   nthreads: Updated number of threads
**   threadid: Our thread id added to list on 1st call 
**
** Return value: thread number (success) or GPTLerror (failure)
*/
static inline int get_thread_num (void)
{
  int t;                   /* logical thread number, defined by array index of found threadid */
  pthread_t mythreadid;    /* thread id from pthreads library */
  int retval = -1;         /* value to return to caller: init to bad value to please compiler */
  bool foundit = false;    /* thread id found in list */
  static const char *thisfunc = "get_thread_num";

  mythreadid = pthread_self ();

  /*
  ** If our thread number has already been set in the list, we are done
  ** VECTOR code should run a bit faster on vector machines.
  */
#define VECTOR
#ifdef VECTOR
  for (t = 0; t < nthreads; ++t)
    if (pthread_equal (mythreadid, threadid[t])) {
      foundit = true;
      retval = t;
    }

  if (foundit)
    return retval;
#else
  for (t = 0; t < nthreads; ++t)
    if (pthread_equal (mythreadid, threadid[t]))
      return t;
#endif

  /* 
  ** Thread id not found. Define a critical region, then start PAPI counters if
  ** necessary and modify threadid[] with our id.
  */
  if (lock_mutex () < 0)
    return GPTLerror ("GPTL: PTHREADS %s: mutex lock failure\n", thisfunc);

  /*
  ** If our thread id is not in the known list, add to it after checking that
  ** we do not have too many threads.
  */
  if (nthreads >= maxthreads) {
    if (unlock_mutex () < 0)
      fprintf (stderr, "GPTL: PTHREADS %s: mutex unlock failure\n", thisfunc);

    return GPTLerror ("THREADED_PTHREADS %s: thread index=%d is too big. Need to invoke \n"
		      "GPTLsetoption(GPTLmaxthreads,value) or recompile GPTL with a\n"
		      "larger value of MAX_THREADS\n", thisfunc, nthreads);
  }

  threadid[nthreads] = mythreadid;

#ifdef VERBOSE
  printf ("PTHREADS %s: 1st call threadid=%lu maps to location %d\n", 
          thisfunc, (unsigned long) mythreadid, nthreads);
#endif

#ifdef HAVE_PAPI

  /*
  ** When HAVE_PAPI is true, if 1 or more PAPI events are enabled,
  ** create and start an event set for the new thread.
  */
  if (GPTLget_npapievents () > 0) {
#ifdef VERBOSE
    printf ("PTHREADS get_thread_num: Starting EventSet threadid=%lu location=%d\n", 
            (unsigned long) mythreadid, nthreads);
#endif
    if (GPTLcreate_and_start_events (nthreads) < 0) {
      if (unlock_mutex () < 0)
        fprintf (stderr, "GPTL: PTHREADS %s: mutex unlock failure\n", thisfunc);

      return GPTLerror ("GPTL: PTHREADS %s: error from GPTLcreate_and_start_events for thread %d\n", 
                        thisfunc, nthreads);
    }
  }
#endif

  /*
  ** IMPORTANT to set return value before unlocking the mutex!!!!
  ** "return nthreads-1" fails occasionally when another thread modifies
  ** nthreads after it gets the mutex!
  */
  retval = nthreads++;

#ifdef VERBOSE
  printf ("PTHREADS get_thread_num: nthreads bumped to %d\n", nthreads);
#endif

  if (unlock_mutex () < 0)
    return GPTLerror ("GPTL: PTHREADS %s: mutex unlock failure\n", thisfunc);

  return retval;
}

/*
** lock_mutex: lock a mutex for private access
*/
static int lock_mutex ()
{
  static const char *thisfunc = "lock_mutex";

  if (pthread_mutex_lock ((pthread_mutex_t *) &t_mutex) != 0)
    return GPTLerror ("GPTL: %s: failure from pthread_lock_mutex\n", thisfunc);

  return 0;
}

/*
** unlock_mutex: unlock a mutex from private access
*/
static int unlock_mutex ()
{
  static const char *thisfunc = "unlock_mutex";

  if (pthread_mutex_unlock ((pthread_mutex_t *) &t_mutex) != 0)
    return GPTLerror ("GPTL: %s: failure from pthread_unlock_mutex\n", thisfunc);
  return 0;
}

static void print_threadmapping (FILE *fp)
{
  int t;

  fprintf (fp, "\n");
  fprintf (fp, "Thread mapping:\n");
  for (t = 0; t < nthreads; ++t)
    fprintf (fp, "threadid[%d] = %lu\n", t, (unsigned long) threadid[t]);
}

/**********************************************************************************/
/*
** Unthreaded case
*/

#else

static int threadinit (void)
{
  static const char *thisfunc = "threadinit";

  if (nthreads != -1)
    return GPTLerror ("GPTL: Unthreaded %s: MUST only be called once", thisfunc);

  nthreads = 0;
  maxthreads = 1;
  return 0;
}

void threadfinalize ()
{
  threadid = -1;
}

static inline int get_thread_num ()
{
  static const char *thisfunc = "get_thread_num";
#ifdef HAVE_PAPI
  /*
  ** When HAVE_PAPI is true, if 1 or more PAPI events are enabled,
  ** create and start an event set for the new thread.
  */
  if (threadid == -1 && GPTLget_npapievents () > 0) {
    if (GPTLcreate_and_start_events (0) < 0)
      return GPTLerror ("GPTL: Unthreaded %s: error from GPTLcreate_and_start_events for thread %0\n",
                        thisfunc);

    threadid = 0;
  }
#endif

  nthreads = 1;
  return 0;
}

static void print_threadmapping (FILE *fp)
{
  fprintf (fp, "\n");
  fprintf (fp, "threadid[0] = 0\n");
}

#endif
