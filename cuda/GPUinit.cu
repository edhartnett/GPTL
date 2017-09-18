// Routine to initialize the GPU
// Author:  Jacques Middlecoff
// Date:  September 2010 
// May 2014 Removed CUT_DEVICE_INIT - Middlecoff
// For Fortran this routine does nothing except return error=0.

#include <stdio.h>
#include <stdlib.h>

extern "C" void gpuinit_ (int *npes,int *me,int *max_accelerators_per_node,int *GPUrun,int *error) {

cudaDeviceProp deviceProp;

*GPUrun = 1;
*error = 0;

#if CUDART_VERSION < 2020
#error "This CUDART version does not support mapped memory!\n"
#endif

// Get properties and verify device 0 supports mapped memory
*error = cudaGetDeviceProperties(&deviceProp, 0);
if(*error != cudaSuccess) {
  printf("GPUinit.cu: cudaGetDeviceProperties error %d \n",*error);
  printf("cudaSuccess,cudaErrorInvalidDevice,cudaErrorDeviceAlreadyInUse %d %d %d \n",cudaSuccess,cudaErrorInvalidDevice,cudaErrorDeviceAlreadyInUse);
  return;
}
if(!deviceProp.canMapHostMemory) {
  printf("GPUinit.cu: Device %d cannot map host memory!\n", 0);
  *error = -88;
  return;
}

*error = cudaSetDevice(*me%*max_accelerators_per_node);
if(*error != cudaSuccess) {
  printf("GPUinit.cu: cudaSetDeviceProperties error %d %d %d \n",*error,*me,*max_accelerators_per_node);
  printf("cudaSuccess, %d \n",cudaSuccess);
  return;
}
printf("MPI rank %d on GPU %d \n",*me,*me%*max_accelerators_per_node);

// set the device flags for mapping host memory
//*error = cudaSetDeviceFlags(cudaDeviceMapHost);
//if(*error != cudaSuccess) {
//  printf("GPUinit.cu: cudaSetDeviceFlags error %d \n",*error);
//  return;
//}
}



