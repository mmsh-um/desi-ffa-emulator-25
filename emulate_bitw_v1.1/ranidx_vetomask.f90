!> last modified 08-07-2023

subroutine ranidx_vetomask(nmax,c,bool,idum,vmsk)
  
  implicit none

  integer(4) :: nmax,c,idum
  logical, dimension(nmax) :: bool,vmsk
  integer(4) :: i,j,k
  integer(4), dimension(nmax) :: avail
  real(4), external :: ran1

  avail=-1
  
  j=1
  do i=1,nmax
     if (vmsk(i)) then
        avail(j)=i
        j=j+1
     endif
  enddo
  
  bool=.false.
  
  k=count(vmsk)
  do i=1,c
     j=int(k*ran1(idum))+1
     bool(avail(j))=.true.
     avail(j:k)=avail(j+1:k+1)
     k=k-1
  enddo

end subroutine ranidx_vetomask
