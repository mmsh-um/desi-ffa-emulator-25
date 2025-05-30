!> last modified 13-01-2015

subroutine advo(n,nmax,k)
  
  implicit none
  
  integer(4) :: n,nmax,k
  real(8) :: pcnt,dt
  real(4) :: t
  real(4), save :: t0
  integer(4), parameter :: nfirst=100

!!$!> parallel version (comment when not compiling with OMP)  
!!$  integer(4), external :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
!!$  integer :: id,nthreads,nmax_eff
!!$
!!$  id=OMP_GET_THREAD_NUM()
!!$  !write(*,*)'id =',id
!!$
!!$  if (id.eq.0) then
!!$     nthreads=OMP_GET_NUM_THREADS()
!!$     !write(*,*)'nthreads =',nthreads
!!$     nmax_eff=nmax/nthreads
!!$     if (n.eq.1) then 
!!$        call cpu_time(t0)
!!$     else
!!$        if (mod(n,nmax_eff/k).eq.0.or.n.eq.nfirst.or.n.eq.nmax_eff) then
!!$           pcnt=dble(n)/nmax_eff*100
!!$           call cpu_time(t)
!!$           dt=dble(t-t0)/nthreads
!!$           write(*,*)'progress [%]             =',real(pcnt)
!!$           write(*,*)' time elapsed [s]        =',real(dt)
!!$           write(*,*)' remaining (lin ext) [s] =',real(dt*100/pcnt-dt)
!!$        endif
!!$     endif
!!$  endif
  
!> serial version (comment when compiling with OMP)
  if (n.eq.1) then 
     call cpu_time(t0)
  else
     if (mod(n,nmax/k).eq.0.or.n.eq.nfirst.or.n.eq.nmax) then
        pcnt=dble(n)/nmax*100
        call cpu_time(t)
        dt=dble(t-t0)
        write(*,*)'progress [%]             =',real(pcnt)
        write(*,*)' time elapsed [s]        =',real(dt)
        write(*,*)' remaining (lin ext) [s] =',real(dt*100/pcnt-dt)
     endif
  endif
  
end subroutine advo
