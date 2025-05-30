!> last modified 31-01-2023

subroutine get_halo_centers_ang(sz,u,w,msk,uh)
  
  implicit none

  include 'PRM_deg_and_rad.f90'
  include 'PRM_llist.f90'
  
  integer(4) :: sz
  real(8), dimension(sz) :: w
  real(8), dimension(4,sz) :: u
  logical, dimension(sz) :: msk
  real(8), dimension(4) :: uh
  integer :: i
  
 !> this is a cartesian mean, it might not be what you want!
  uh=0.d0
  do i=1,sz
     if (msk(i)) then
        uh(1)=uh(1)+w(i)*u(4,i)*u(1,i)
        uh(2)=uh(2)+w(i)*u(4,i)*u(2,i)
        uh(3)=uh(3)+w(i)*u(4,i)*u(3,i)
     endif
  enddo
  uh=uh/count(msk)
  uh(4)=sqrt(uh(1)**2+uh(2)**2+uh(3)**2)
  if (uh(4).gt.0.d0) uh(1:3)=uh(1:3)/uh(4)
  
end subroutine get_halo_centers_ang
   
