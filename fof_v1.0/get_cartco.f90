!> last modified 05-02-2020

subroutine get_cartco(n,ra,dec,rshift,x,y,z,write_cartco)
  
  implicit none

  include 'PRM_string_attributes.f90'
  
  integer(4) :: n
  real(8), dimension(n) :: ra,dec,rshift
  real(8), dimension(n) :: x,y,z
  logical :: write_cartco
  real(8) :: d
  real(8), external :: rshift2dist_integrand
  integer(4) :: i
  
  do i=1,n
     !d=1.d0
     call qgaus(rshift2dist_integrand,0.d0,rshift(i),d)
     call cartco(x(i),y(i),z(i),ra(i),dec(i),d)
  enddo
  
  if (write_cartco) then
     write(*,*)' writing cartesian coordinates to a file ...'
     open(2363,file='out/cartco_test.dat')
     do i=1,n
        write(2363,*)real(x(i)),real(y(i)),real(z(i))
     enddo
     close(2363)
     write(*,*)' done'
  endif

end subroutine get_cartco
