!> last modified 21-11-2018

subroutine smplmsk(n,we,msk,smpl)
  
  implicit none
  
  include 'PRM_we.f90'
  !include 'PRM_smpl.f90'

  integer(4) :: n,smpl
  integer(itype), dimension(n,nwe) :: we
  logical, dimension(n) :: msk
  integer(4) :: i,k,j
  integer(4) :: nsel
  logical, dimension(nwe,itype*8-1) :: lt
  logical, dimension(nbits) :: l
  
  !write(*,*)' computing mask ...'

  do i=1,n
     do k=1,nwe
        do j=1,itype*8-1
           !lt(k,j)=btest(we(i,k),j-1)
           l(j+(k-1)*(itype*8-1))=btest(we(i,k),j-1)
        enddo
     enddo
     if (.not.l(smpl)) then
        !w(i)=0.d0
        msk(i)=.false.
     endif
  enddo

  !write(*,*)' done'
  
end subroutine smplmsk
