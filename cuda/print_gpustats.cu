#include <stdio.h>
#include <unistd.h>
#ifdef HAVE_MPI
#include <mpi.h>
#endif
#include "../devicehost.h"
#include "./private.h"

//FIX THIS: It's also in ../private.h
#define PRTHRESH 1000000L

extern "C" {

__host__ void GPTLprint_gpustats (int nwarps_found, // From gptlget_gpusizes
			 int nwarps_timed, // From gptlget_gpusizes
			 // From gptlget_overhead_gpu:
			 long long ftn_ohdgpu,            // Fortran wrapper overhead
			 long long get_thread_num_ohdgpu, /* Getting my thread index */
			 long long genhashidx_ohdgpu,     /* Generating hash index */
			 long long getentry_ohdgpu,       /* Finding entry in hash table */
			 long long utr_ohdgpu,            /* Underlying timing routine */
			 long long self_ohdgpu,           // Cost est. for timing this region
			 long long parent_ohdgpu,         // Cost est. to parent of this region
			 // From gptlfill_gpustats:
			 Gpustats gpustats[MAX_GPUTIMERS],
			 int max_name_len_gpu,
			 int ngputimers,
			 // From gptlget_memstats_gpu:
			 float hashmem,
			 float regionmem)
{
  FILE *fp;                    // output file for GPU stats
  int khz;                     // clock rate returned from GPTL_get_gpu_props
  int warpsize;                // warp size returned from GPTL_get_gpu_props
  double gpu_hz;               // khz turned into float hz
  int maxthreads_gpu; 
  int devnum;

  int count_max, count_min;
  int extraspace;
  int i, n;
  int ret;
  double wallmax, wallmin;
  double self, parent;
  double tot_ohdgpu;
  int myrank = 0;
  int mpi_active;
  char outfile[14];
#define HOSTSIZE 32
  char hostname[HOSTSIZE];
  static const char *thisfunc = "GPTLprint_gpustats";

#ifdef HAVE_MPI
  ret = MPI_Initialized (&mpi_active);
  if (mpi_active)
    ret = MPI_Comm_rank (MPI_COMM_WORLD, &myrank);
#endif

  ret = GPTLget_gpu_props (&khz, &warpsize, &devnum);
  gpu_hz = khz * 1000.;

  sprintf (outfile, "timing.%d", myrank);
  if ( ! (fp = fopen (outfile, "a")))
    fp = stderr;
  fprintf (fp, "\n\nGPU Results:\n");
  fprintf (fp, "%s: device number=%d\n", thisfunc, devnum);
  ret = gethostname (hostname, HOSTSIZE);
  fprintf (fp, "%s: hostname=%s\n", thisfunc, hostname);

  fprintf (fp, "Underlying timing routine was clock64()\n");
  tot_ohdgpu = (ftn_ohdgpu + get_thread_num_ohdgpu + genhashidx_ohdgpu + 
		getentry_ohdgpu + utr_ohdgpu) / gpu_hz;
  fprintf (fp, "Total overhead of 1 GPTLstart_gpu or GPTLstop_gpu call=%g seconds\n", tot_ohdgpu);
  fprintf (fp, "Components are as follows:\n");
  fprintf (fp, "Fortran layer:             %7.1e = %5.1f%% of total\n", 
	   ftn_ohdgpu / gpu_hz, ftn_ohdgpu * 100. / (tot_ohdgpu * gpu_hz) );
  fprintf (fp, "Get thread number:         %7.1e = %5.1f%% of total\n", 
	   get_thread_num_ohdgpu / gpu_hz, get_thread_num_ohdgpu * 100. / (tot_ohdgpu * gpu_hz) );
  fprintf (fp, "Generate hash index:       %7.1e = %5.1f%% of total\n", 
	   genhashidx_ohdgpu / gpu_hz, genhashidx_ohdgpu * 100. / (tot_ohdgpu * gpu_hz) );
  fprintf (fp, "Find hashtable entry:      %7.1e = %5.1f%% of total\n", 
	   getentry_ohdgpu / gpu_hz, getentry_ohdgpu * 100. / (tot_ohdgpu * gpu_hz) );
  fprintf (fp, "Underlying timing routine: %7.1e = %5.1f%% of total\n", 
	   utr_ohdgpu / gpu_hz, utr_ohdgpu * 100. / (tot_ohdgpu * gpu_hz) );
  fprintf (fp, "\nGPU timing stats\n");
  fprintf (fp, "GPTL could handle up to %d warps (%d threads)\n", 
	   maxthreads_gpu / warpsize, maxthreads_gpu);
  fprintf (fp, "This setting can be changed with: GPTLsetoption(GPTLmaxthreads_gpu,<number>)\n");
  fprintf (fp, "%d warps were found\n", nwarps_found);
  fprintf (fp, "%d warps were timed\n", nwarps_timed);
  fprintf (fp, "Only warps which were timed are counted in the following stats\n");
  fprintf (fp, "Overhead estimates self_OH and parent_OH are for warp with \'maxcount\' calls\n");
  fprintf (fp, "OHD estimate assumes Fortran, and non-handle routines used\n");
  fprintf (fp, "Actual overhead can be reduced by using \'handle\' routines and \'_c\' Fortran routines\n");
  // Print header, padding to length of longest name
  extraspace = max_name_len_gpu - 4; // "name" is 4 chars
  for (i = 0; i < extraspace; ++i)
    fprintf (fp, " ");
  fprintf (fp, "name calls warps  wallmax (warp) wallmin (warp) maxcount (warp) mincount (warp) self_OH parent_OH\n");
  for (n = 0; n < ngputimers; ++n) {
    extraspace = max_name_len_gpu - strlen (gpustats[n].name);
    for (i = 0; i < extraspace; ++i)
      fprintf (fp, " ");
    fprintf (fp, "%s ", gpustats[n].name);             // regopm name
    fprintf (fp, "%5d ", gpustats[n].count);           // # start/stops of region 
    fprintf (fp, "%5d ", gpustats[n].nwarps);          // nwarps_timed involving name
    
    wallmax = gpustats[n].accum_max / gpu_hz;          // max time for name across warps
    if (wallmax < 0.01)
      fprintf (fp, "%8.2e ", wallmax);
    else
      fprintf (fp, "%8.3f ", wallmax);
    fprintf (fp, "%5d ",gpustats[n].accum_max_warp);   // warp number for max
    
    wallmin = gpustats[n].accum_min / gpu_hz;          // min time for name across warps
    if (wallmin < 0.01)
      fprintf (fp, "%8.2e ", wallmin);
    else
      fprintf (fp, "%8.3f ", wallmin);	       
    fprintf (fp, "%5d ",gpustats[n].accum_min_warp);   // warp number for min
    
    count_max = gpustats[n].count_max;
    if (count_max < PRTHRESH)
      fprintf (fp, "%9lu ", count_max);                // max count for region "name"
    else
      fprintf (fp, "%9.1e ", (float) count_max);
    fprintf (fp, "%5d ",gpustats[n].count_max_warp);   // warp which accounted for max times
    
    count_min = gpustats[n].count_min;                
    if (count_min < PRTHRESH)
      fprintf (fp, "%9lu ", count_min);                // min count for region "name"
    else
      fprintf (fp, "%9.1e ", (float) count_min);
    fprintf (fp, "%5d ",gpustats[n].count_min_warp);   // warp which accounted for max times

    self = gpustats[n].count_max * self_ohdgpu / gpu_hz;     // self ohd est
    if (self < 0.01)
      fprintf (fp, "%8.2e  ", self);
    else
      fprintf (fp, "%8.3f  ", self);	       
    
    parent = gpustats[n].count_max * parent_ohdgpu / gpu_hz; // parent ohd est
    if (self < 0.01)
      fprintf (fp, "%8.2e ", parent);
    else
      fprintf (fp, "%8.3f ", parent);	       
    
    fprintf (fp, "\n");
  }
  
  fprintf (fp, "\n");
  fprintf (fp, "Total GPTL GPU memory usage = %g KB\n", (hashmem + regionmem)*.001);
  fprintf (fp, "Components:\n");
  fprintf (fp, "Hashmem                     = %g KB\n" 
               "Regionmem                   = %g KB\n", hashmem*.001, regionmem*.001);
}

}
