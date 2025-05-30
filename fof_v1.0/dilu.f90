!> last modified 08-04-2023

subroutine dilu(npart,msk,f,idum)
  
  implicit none

  integer(4) :: npart,idum
  real(8) :: f
  logical, dimension(npart) :: msk
  real(4), external :: ran1
  integer(4) :: i

  do i=1,npart
     if (msk(i)) then
        if (ran1(idum).gt.f) msk(i)=.false.
     endif
  enddo

end subroutine dilu
