program persist
  use gptl
  use gptl_acc
  implicit none
!$acc routine (gptlinit_handle_gpu) seq
!$acc routine (doalot) seq
!$acc routine (doalot2) seq

  integer :: ret
  integer :: n
  integer :: verbose = 1
  integer :: maxthreads_gpu = 3584
  integer :: tablesize_gpu = 63
  integer :: outerlooplen = 100000
  integer :: innerlooplen = 100
  integer :: ans
  integer :: handle, handle2
  real, allocatable :: vals(:)
  real, external :: doalot, doalot2

  call getval (maxthreads_gpu, 'maxthreads_gpu')
  call getval (outerlooplen, 'outerlooplen')
  call getval (innerlooplen, 'innerlooplen')
  allocate (vals(outerlooplen))

!JR NOTE: gptlinitialize call increases mallocable memory size on GPU. That call will fail
!JR if any GPU activity happens before the call to gptlinitialize
  ret = gptlsetoption (gptlmaxthreads_gpu, maxthreads_gpu)
  write(6,*)'persist: calling gptlinitialize'
  ret = gptlinitialize ()
  write(6,*)'persist: calling gptlinitialize_gpu'
!$acc kernels copyin(verbose, tablesize_gpu, maxthreads_gpu) copyout(ret)
  ret = gptlinitialize_gpu (verbose, tablesize_gpu, maxthreads_gpu)
!$acc end kernels
  if (ret == 0) then
    write(6,*) 'Successful return from GPTLinitialize_gpu'
  else
    write(6,*) 'Failure from GPTLinitialize_gpu'
  end if

!JR Need to call GPU-specific init_handle routine because its tablesize may differ from CPU
!$acc kernels copyout(ret,handle,handle2)
  ret = gptlinit_handle_gpu ('doalot_handle_sqrt_c', handle)
  ret = gptlinit_handle_gpu ('a', handle2)
!$acc end kernels

  ret = gptlstart ('doalot_cpu')
!$acc parallel loop copyin(handle,handle2) copyout(ret, vals)
  do n=1,outerlooplen
    ret = gptlstart_gpu ('doalot_log')
    vals(n) = doalot (n, innerlooplen)
    ret = gptlstop_gpu ('doalot_log')

    ret = gptlstart_gpu ('doalot_sqrt')
    vals(n) = doalot2 (n, innerlooplen)
    ret = gptlstop_gpu ('doalot_sqrt')

    ret = gptlstart_gpu_c ('doalot_sqrt_c'//char(0))
    vals(n) = doalot2 (n, innerlooplen)
    ret = gptlstop_gpu_c ('doalot_sqrt_c'//char(0))

    ret = gptlstart_handle_gpu_c ('doalot_handle_sqrt_c'//char(0), handle)
    vals(n) = doalot2 (n, innerlooplen)
    ret = gptlstop_handle_gpu_c ('doalot_handle_sqrt_c'//char(0), handle)

    ret = gptlstart_handle_gpu_c ('a'//char(0), handle2)
    vals(n) = doalot2 (n, innerlooplen)
    ret = gptlstop_handle_gpu_c ('a'//char(0), handle2)
  end do
!$acc end parallel
  ret = gptlstop ('doalot_cpu')

  ret = gptlstart ('doalot_cpu_nogputimers')
!$acc parallel loop copyout(vals)
  do n=1,outerlooplen
    vals(n) = doalot (n, innerlooplen)
    vals(n) = doalot2 (n, innerlooplen)
    vals(n) = doalot2 (n, innerlooplen)
    vals(n) = doalot2 (n, innerlooplen)
    vals(n) = doalot2 (n, innerlooplen)
  end do
!$acc end parallel

  ret = gptlstop ('doalot_cpu_nogputimers')

  ret = gptlpr (0)
  ret = gptlpr_gpu ()
end program persist

real function doalot (n, innerlooplen) result (sum)
  implicit none
  integer, intent(in) :: n, innerlooplen
  integer :: i, iter
  real :: sum
!$acc routine seq

  sum = 0.
  do iter=1,innerlooplen
    do i=1,n
      sum = sum + log (real (iter*i))
    end do
  end do
end function doalot

real function doalot2 (n, innerlooplen) result (sum)
  implicit none
  integer, intent(in) :: n, innerlooplen
  integer :: i, iter
  real :: sum
!$acc routine seq

  sum = 0.
  do iter=1,innerlooplen
    do i=1,n
      sum = sum + sqrt (real (iter*i))
    end do
  end do
end function doalot2

subroutine getval (arg, str)
  implicit none

  integer, intent(inout) :: arg
  character(len=*), intent(in) :: str

  integer :: ans

  write(6,*)'Enter ',str,' or -1 to accept default (',arg,')'
  read(5,*) ans
  if (ans /= -1) then
    arg = ans
  end if
  write(6,*) str,'=',arg
end subroutine getval
