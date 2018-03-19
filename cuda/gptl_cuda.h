/*
** $Id: gptl_cuda.h.template,v 1.3 2011-03-28 20:55:19 rosinski Exp $
**
** Author: Jim Rosinski
**
** GPTL header file to be included in user code
*/

#ifndef GPTL_CUDA_H
#define GPTL_CUDA_H

/*
** User-visible function prototypes
*/

extern "C" {
__device__ extern int GPTLstart_gpu (const char *);
__device__ extern int GPTLinit_handle_gpu (const char *, int *);
__device__ extern int GPTLstart_handle_gpu (const char *, int *);
__device__ extern int GPTLstop_gpu (const char *);
__device__ extern int GPTLstop_handle_gpu (const char *, const int *);
__device__ extern void GPTLdummy_gpu (int);
__device__ extern int GPTLmy_sleep (float);
};
#endif
