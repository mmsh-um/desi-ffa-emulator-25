!> last modified 24-10-2016

subroutine l2we(n,l,we)
  
  implicit none
  
  include 'PRM_we.f90'
  
  integer(4) :: n
  logical, dimension(n,nbits) :: l
  integer(itype), dimension(n,nwe) :: we
  integer(4) :: i,j,k
  logical, dimension(itype*8-1) :: lt
  
  do i=1,n
     !call advo(i,n,10)
     !call advs(i,n,10)
     do k=1,nwe
        !write(*,*)(k-1)*(itype*8-1)+1,k*(itype*8-1)
        lt=l(i,(k-1)*(itype*8-1)+1:k*(itype*8-1))
        we(i,k)=0
        do j=1,itype*8-1
           if (lt(j)) we(i,k)=we(i,k)+2**(j-1)
        enddo
     enddo
  enddo
  
end subroutine l2we
