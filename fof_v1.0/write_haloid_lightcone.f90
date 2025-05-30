!> last modified 04-06-2023

subroutine write_haloid_lightcone(n,ra,dec,rshift,w,msk,haloid,cfc,namout_part)
  
  implicit none

  include 'PRM_string_attributes.f90'

  integer(4) :: n
  real(8), dimension(n) :: ra,dec,rshift,w
  integer(4), dimension(n) :: haloid,cfc
  logical, dimension(n) :: msk
  character(msl) :: namout_part
  integer(4) :: i

  open(100,file=trim(namout_part))
  do i=1,n
     call advo(i,n,5)
     !write(100,*)ra(i),dec(i),rshift(i),haloid(i),w(i),msk(i),cfc(i)
     write(100,*)real(ra(i)),real(dec(i)),real(rshift(i)),haloid(i),real(w(i)),msk(i),cfc(i)
     !write(100,*)halo(i)
  enddo
  close(100)
     
end subroutine write_haloid_lightcone
