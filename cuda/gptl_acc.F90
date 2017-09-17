! GPTL module file for user code and private code
module gptl_acc
  use iso_c_binding
#include "../defs.h"
  implicit none
  public
! Private routines
  private :: gptlget_gpusizes, gptlget_overhead_gpu, gptlget_memstats_gpu
             
! Private data
  private :: gpustats_t

! CRITICAL: These items MUST match their C counterparts in devicehost.h!!!!!
! Function prototypes: Those without character arguments bind directly to C or CUDA routines
! For some reason, "use iso_c_binding" needs to be repeated inside interface routines
  type gpustats_t
    integer(c_long_long) :: accum_max
    integer(c_long_long) :: accum_min
    integer(c_long_long) :: max
    integer(c_long_long) :: min
    integer(c_long)      :: count
    integer(c_int)       :: accum_max_warp
    integer(c_int)       :: accum_min_warp
    integer(c_int)       :: nwarps
    integer(c_int)       :: count_max
    integer(c_int)       :: count_max_warp
    integer(c_int)       :: count_min
    integer(c_int)       :: count_min_warp
    character(kind=c_char,len=MAX_CHARS+1) :: name
  end type gpustats_t

  interface
    integer(c_int) function gptldummy_gpu () &
         bind(C,name="GPTLdummy_gpu")
!$acc routine seq
    end function gptldummy_gpu

    integer(c_int) function gptlinitialize_gpu (verbose_in, tablesize_in, maxthreads_in) &
         bind(C,name="GPTLinitialize_gpu")
      use iso_c_binding
!$acc routine seq
      integer(c_int), value, intent(in) :: verbose_in
      integer(c_int), value, intent(in) :: tablesize_in
      integer(c_int), value, intent(in) :: maxthreads_in
    end function gptlinitialize_gpu

    integer function gptlstart_gpu (name)
!$acc routine seq
      character(len=*) :: name
    end function gptlstart_gpu

    integer function gptlstart_gpu_c (name)
!$acc routine seq
      character(len=*) :: name
    end function gptlstart_gpu_c

    integer function gptlinit_handle_gpu (name, handle)
!$acc routine seq
      character(len=*) :: name
      integer :: handle
    end function gptlinit_handle_gpu

    integer function gptlstart_handle_gpu (name, handle)
!$acc routine seq
      character(len=*) :: name
      integer :: handle
    end function gptlstart_handle_gpu

    integer function gptlstart_handle_gpu_c (name, handle)
!$acc routine seq
      character(len=*) :: name
      integer :: handle
    end function gptlstart_handle_gpu_c

    integer function gptlstop_gpu (name)
!$acc routine seq
      character(len=*) :: name
    end function gptlstop_gpu

    integer function gptlstop_gpu_c (name)
!$acc routine seq
      character(len=*) :: name
    end function gptlstop_gpu_c

    integer function gptlstop_handle_gpu (name, handle)
!$acc routine seq
      character(len=*) :: name
      integer :: handle
    end function gptlstop_handle_gpu

    integer function gptlstop_handle_gpu_c (name, handle)
!$acc routine seq
      character(len=*) :: name
      integer :: handle
    end function gptlstop_handle_gpu_c

! These functions wrap CUDA, needed to get around the C-acc bug
    integer(c_int) function gptlget_gpusizes (nwarps_found, nwarps_timed) &
         bind(C,name="GPTLget_gpusizes")
      use iso_c_binding
!$acc routine seq
      integer(c_int), intent(out) :: nwarps_found
      integer(c_int), intent(out) :: nwarps_timed
    end function gptlget_gpusizes

    integer(c_int) function gptlget_overhead_gpu (ftn_ohdgpu, &
                                                  get_thread_num_ohdgpu, &
                                                  genhashidx_ohdgpu, &
                                                  getentry_ohdgpu, &
                                                  utr_ohdgpu, &
                                                  self_ohdgpu, &
                                                  parent_ohdgpu) &
         bind(C,name="GPTLget_overhead_gpu")
      use iso_c_binding
!$acc routine seq
      integer(c_long_long), intent(out) :: ftn_ohdgpu
      integer(c_long_long), intent(out) :: get_thread_num_ohdgpu
      integer(c_long_long), intent(out) :: genhashidx_ohdgpu
      integer(c_long_long), intent(out) :: getentry_ohdgpu
      integer(c_long_long), intent(out) :: utr_ohdgpu
      integer(c_long_long), intent(out) :: self_ohdgpu
      integer(c_long_long), intent(out) :: parent_ohdgpu
    end function gptlget_overhead_gpu

    integer(c_int) function gptlget_memstats_gpu (hashmem, regionmem) &
         bind(C,name="GPTLget_memstats_gpu")
      use iso_c_binding
!$acc routine seq
      real(c_float), intent(out) :: hashmem
      real(c_float), intent(out) :: regionmem
    end function gptlget_memstats_gpu
  end interface

CONTAINS

! This function is needed because invoking CUDA from C seems to be broken
  integer function gptlpr_gpu ()
    use iso_c_binding
    ! Returned from GPTLget_gpusizes:
    integer(c_int) :: nwarps_found
    integer(c_int) :: nwarps_timed
    integer(c_int) :: max_name_len_gpu
    integer(c_int) :: ngputimers
    
    ! Returned from GPTLget_overhead_gpu:
    integer(c_long_long) :: ftn_ohdgpu            ! Fortran wrapper overhead
    integer(c_long_long) :: get_thread_num_ohdgpu ! Getting my thread index
    integer(c_long_long) :: genhashidx_ohdgpu     ! Generating hash index
    integer(c_long_long) :: getentry_ohdgpu       ! Finding entry in hash table
    integer(c_long_long) :: utr_ohdgpu            ! Underlying timing routine
    integer(c_long_long) :: self_ohdgpu           ! Cost est. for timing this region
    integer(c_long_long) :: parent_ohdgpu         ! Cost est. to parent of this region
    
    ! Returned from GPTLget_memstats_gpu:
    real(c_float) :: hashmem
    real(c_float) :: regionmem

    integer :: ret
    type(gpustats_t) :: gpustats(MAX_GPUTIMERS)

    integer, external :: gptlfill_gpustats
!$acc routine (gptlfill_gpustats) seq

!$acc kernels copyout(ret, nwarps_found, nwarps_timed)
    ret = gptlget_gpusizes (nwarps_found, nwarps_timed)
!$acc end kernels

!$acc kernels copyout(ret, ftn_ohdgpu, get_thread_num_ohdgpu, genhashidx_ohdgpu, &
!$acc&		      getentry_ohdgpu, utr_ohdgpu, self_ohdgpu, parent_ohdgpu)     
    ret = gptlget_overhead_gpu (ftn_ohdgpu,             &
		                get_thread_num_ohdgpu,  &
                                genhashidx_ohdgpu,  &
                                getentry_ohdgpu,  &
                                utr_ohdgpu,  &
                                self_ohdgpu,  &
                                parent_ohdgpu)
!$acc end kernels

!$acc kernels copyout(ret, gpustats, max_name_len_gpu, ngputimers)
    ret = gptlfill_gpustats (gpustats, max_name_len_gpu, ngputimers)
!$acc end kernels

!$acc kernels copyout(ret, hashmem, regionmem)
    ret = gptlget_memstats_gpu (hashmem, regionmem)
!$acc end kernels

! Now that we have all the data, call C wrapper function to print the results 
! (calls GPTLprint_gpustats)
    call gptlprint_gpustats (nwarps_found, nwarps_timed, &
                             ftn_ohdgpu, get_thread_num_ohdgpu, genhashidx_ohdgpu, &
                             getentry_ohdgpu, utr_ohdgpu, self_ohdgpu, parent_ohdgpu, &
                             gpustats, max_name_len_gpu, ngputimers, &
                             hashmem, regionmem)
  end function gptlpr_gpu
end module gptl_acc
