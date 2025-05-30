!> last modified 08-04-2023

subroutine split(npart,msk,nsplit,idum,group)
  
  implicit none

  integer(4) :: npart,idum,nsplit
  logical, dimension(npart) :: msk
  integer, dimension(npart) :: group
  real(4), external :: ran1
  integer(4) :: i

  group=-1
  
  do i=1,npart
     if (msk(i)) then
        group(i)=int(nsplit*ran1(idum))+1
     endif
  enddo

end subroutine split
