!> last modified 08-02-2023

subroutine write_wemu(n,we,pfx)
  
  implicit none

  include 'PRM_we.f90'
  include 'PRM_string_attributes.f90'

  integer(4) :: n
  integer(itype), dimension(n,nwe) :: we
  character(msl) :: pfx
  integer(4) :: i,j,m

  open(100,file=trim(pfx)//'wemu_unformatted.dat',form='unformatted')
  write(100)we
  close(100)
  
!!$  open(100,file=trim(pfx)//'wemu.dat')
!!$  do i=1,n
!!$     write(100,*)(we(i,m),m=1,nwe)
!!$  enddo
!!$  close(100)
  
end subroutine write_wemu
