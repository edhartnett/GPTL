/*
** $Id$
**
** Author: Jim Rosinski
**
** Contains structures shared between CPU and GPU
*/

#ifndef GPTL_DH
#define GPTL_DH

#include "./defs.h"

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
