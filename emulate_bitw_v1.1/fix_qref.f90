!> last modified 06-04-2023

subroutine fix_qref(qref,sz0,sg_sz,ntm0,sg_ntm,write_fix_lg)
  
  implicit none

  include 'PRM_selprop_bins.f90'

  real(8), dimension(nlogbins_sz,nlogbins_ntm) :: qref
  real(8) :: sz0,sg_sz,ntm0,sg_ntm
  logical :: write_fix_lg
  integer(4) :: i,j
  real(8) :: sz,ntm
  real(8) :: norm,qext,a
  real(8) :: r,sg_r,x,sg_x,y,sg_y,theta
  real(8), external :: sigmoid
  real(4), external :: gasdev
  real(8), parameter :: flex_pos=1.45d0
  real(8), parameter :: steepness=0.76d0
  
  norm=sigmoid(-flex_pos/steepness)

!!$  do i=1,nlogbins_sz
!!$     sz=10**(zerolog_sz+(i-1)*logbinsz_sz)
!!$     do j=1,nlogbins_ntm
!!$        ntm=10**(zerolog_ntm+(j-1)*logbinsz_ntm)
!!$        qext=1/(1-norm)*(sigmoid((ntm-flex_pos)/steepness)-norm)  
!!$        a=sigmoid((sz-sz0)/sg_sz)*sigmoid((ntm-ntm0)/sg_ntm)    
!!$        qref(i,j)=(1-a)*qref(i,j)+a*qext
!!$     enddo
!!$  enddo

  sg_x=sg_sz/sz0
  sg_y=sg_ntm/ntm0
  !sg_r=sqrt(0.5*((sg_sz/sz0)**2+(sg_ntm/ntm0)**2))
  do i=1,nlogbins_sz
     sz=10**(zerolog_sz+(i-1)*logbinsz_sz)
     do j=1,nlogbins_ntm
        ntm=10**(zerolog_ntm+(j-1)*logbinsz_ntm)
        qext=1/(1-norm)*(sigmoid((ntm-flex_pos)/steepness)-norm)  
        x=sz/sz0
        y=ntm/ntm0
        r=sqrt(x**2+y**2)
        theta=acos(x/r)
        sg_r=cos(theta)**2*sg_x+sin(theta)**2*sg_y
        a=sigmoid((r-1)/sg_r)
        qref(i,j)=(1-a)*qref(i,j)+a*qext
     enddo
  enddo
  
  if (write_fix_lg) then
     open(543,file='out/qref_fix.dat')
     do i=1,nlogbins_sz
        sz=10**(zerolog_sz+(i-1)*logbinsz_sz)
        do j=1,nlogbins_ntm
           ntm=10**(zerolog_ntm+(j-1)*logbinsz_ntm)   
           write(543,*)sz,ntm,qref(i,j)
        enddo
        write(543,*)
     enddo
     close(543)
  endif
     
end subroutine fix_qref
