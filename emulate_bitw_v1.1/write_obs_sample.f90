!> last modified 02-02-2023

subroutine write_obs_sample(n,ra,dec,z,msk,w,namout)
  
  implicit none

  include 'PRM_string_attributes.f90'

  integer(4) :: n
  real(8), dimension(n) :: ra,dec,z,w
  logical, dimension(n) :: msk
  character(msl) :: namout
  integer(4) :: i
  
  open(180,file=trim(namout))
  do i=1,n
     write(180,*)real(ra(i)),real(dec(i)),real(z(i)),msk(i),real(w(i))
  enddo
  close(180)
  
end subroutine write_obs_sample
