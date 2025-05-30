!> last modified 18-01-2022

subroutine we2l(n,we,l)
  
  implicit none
  
  include 'PRM_we.f90'

  integer(4) :: n
  integer(itype), dimension(n,nwe) :: we
  logical, dimension(n,nbits) :: l
  integer(4) :: i,k,j

  do i=1,n
     do k=1,nwe
        do j=1,itype*8-1
           l(i,j+(k-1)*(itype*8-1))=btest(we(i,k),j-1)
        enddo
     enddo
  enddo
  
end subroutine we2l
