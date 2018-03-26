/*
** $Id: gptl_acc.h.template,v 1.3 2011-03-28 20:55:19 rosinski Exp $
**
** Author: Jim Rosinski
**
** GPTL header file to be included in user code
*/

#ifndef GPTL_ACC_H
#define GPTL_ACC_H

/*
** User-visible function prototypes
*/

extern "C" {
#pragma acc routine seq
__device__ int GPTLinit_handle_gpu (const char *, int *);
#pragma acc routine seq
__device__ int GPTLstart_gpu (const char *);
#pragma acc routine seq
__device__ int GPTLstart_handle_gpu (const char *, int *);
#pragma acc routine seq
__device__ int GPTLstop_gpu (const char *);
#pragma acc routine seq
__device__ int GPTLstop_handle_gpu (const char *, const int *);
#pragma acc routine seq
__device__ int GPTLmy_sleep (float);
};
#endif
