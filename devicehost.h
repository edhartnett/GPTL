/*
** $Id$
**
** Author: Jim Rosinski
**
** Contains definitions shared between CPU and GPU
*/

/* longest timer name allowed (probably safe to just change) */
#ifndef GPTL_DH
#define GPTL_DH

#define SUCCESS 0
#define FAILURE -1

#define MAX_CHARS 63
#ifdef ENABLE_ACC
// Warpsize will be verified by the library
#define WARPSIZE 32
// Pascal: 56 SMs 64 cuda cores each = 3584 cores
#define DEFAULT_MAXTHREADS_GPU 14336
#define DEFAULT_TABLE_SIZE_GPU 63
#define MAX_GPUTIMERS 50

typedef struct {
  long long accum_max;
  long long accum_min;
  long long max;
  long long min;
  unsigned long count;
  int accum_max_warp;
  int accum_min_warp;
  int nwarps;
  int count_max;
  int count_max_warp;
  int count_min;
  int count_min_warp;
  char name[MAX_CHARS+1];
} Gpustats;

#endif
#endif
