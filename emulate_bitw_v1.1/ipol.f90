!> last modified 14-01-2011

function ipol(r,imax,rd,fd)
  
  implicit none
  
  real(8) :: ipol,r
  integer(4) :: imax
  real(8), dimension(imax) :: rd,fd
  integer(4) :: i
  real(8) :: r1,r2,r0,gm
  real(8) :: m,q
  !integer(4), external :: sgn
  real(8), parameter :: cost_check=1.d-3
  real(8), parameter :: gm_max=1.d20
  real(8), parameter :: r0_min=1.d-20
  real(8), parameter :: r0_max=1.d20

!!$  integer(4), parameter :: nmax=1000
!!$  real(8) :: r_sh(nmax),f_sh(nmax)
!!$  integer(4) :: n_sh
!!$  common /ipol_sh/ r_sh,f_sh,n_sh

!!$  if (n_sh.gt.nmax) then
!!$     write(*,*)'n_sh > nmax in function -ipol-'
!!$     write(*,*)'program paused, press ENTER to continue' 
!!$     read(*,*)
!!$  endif

  do i=2,imax-1
     if (r.lt.rd(i)) goto 100
  enddo
100 continue
  !write(*,*)'i =',i
  
  r2=rd(i)
  r1=rd(i-1)   
!!$  write(*,*)'r2 =',r2,'~~>',fd(i+1)
!!$  write(*,*)'r =',r
!!$  write(*,*)'r1 =',r1,'~~>',fd(i)

  gm=-(log(fd(i-1))-log(fd(i)))/(log(r1)-log(r2))
  r0=r1*fd(i-1)**(1.d0/gm)
!!$  write(*,*)'gm =',gm
!!$  write(*,*)'r0 =',r0
!!$  write(*,*)
  m=(fd(i-1)-fd(i))/(r1-r2)
  q=fd(i-1)-m*r1
!!$  write(*,*)'m =',m
!!$  write(*,*)'q =',q
!!$  write(*,*)

  !if (sgn(fd(i-1)).ne.sgn(fd(i))) then
  if ((isNaN(r0).eqv..false.).and.(isNaN(gm).eqv..false.).and.(abs(gm).lt.gm_max).and.(abs(r0).gt.r0_min).and.(abs(r0).lt.r0_max).and.(abs(abs(fd(i-1)/fd(i))-1.d0).gt.cost_check)) then
     !write(*,*)'power-law'
     ipol=(r/r0)**(-gm)
  else
     !write(*,*)'straigth'
     ipol=m*r+q
  endif
  
end function ipol
