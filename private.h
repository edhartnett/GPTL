/*
$Id: private.h,v 1.7 2004-10-14 21:52:03 rosinski Exp $
*/

#include "gpt.h"

#ifndef MIN
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#endif

#ifndef MAX
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#endif

#define STRMATCH(X,Y) (strcmp((X),(Y)) == 0)
#define MAX_CHARS 15

typedef enum {false = 0, true = 1} bool;

typedef struct {
  long last_sec;
  long last_usec;
  long accum_sec;
  long accum_usec;
  float max;
  float min;
  float overhead;
} Wallstats;

typedef struct {
  long last_utime;
  long last_stime;
  long accum_utime;
  long accum_stime;
} Cpustats;

typedef struct {
  int some_compilers_dont_allow_empty_structs;
} Auxstats;
  
typedef struct TIMER {
  char *name;
  int depth;
  long count;
  int *max_depth;         /* max depth in timer tree (for indentation) */
  int *current_depth;     /* current depth in timer tree (for indentation) */
  int indent_level;
  Wallstats wall;
  Cpustats cpu;
  Auxstats aux;
  bool gather_wall;
  bool gather_cpu;
  bool gather_aux;
  bool onflg;
  struct TIMER *next;
} Timer;

typedef struct {
  const Option option;
  const char *name;
  const char *str;
  bool enabled;
} Settings;

/* Function prototypes */

extern int GPTerror (const char *, ...);
