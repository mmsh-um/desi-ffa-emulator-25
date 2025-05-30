!> last modified 31-01-2023

subroutine get_nhalo(npart,msk,haloid,thold,nhalo)

  implicit none

  integer(4) :: npart,thold,nhalo
  integer(4), dimension(npart) :: haloid
  logical, dimension(npart) :: msk
  integer(4), dimension(:), allocatable :: cnt
  integer(4) :: i
  
  allocate(cnt(npart))
  
  cnt=0
  do i=1,npart
     if (msk(i)) cnt(haloid(i))=cnt(haloid(i))+1
  enddo
  nhalo=count(cnt.ge.thold)
  write(*,*)' sum(cnt) =',sum(cnt)
  write(*,*)' nhalo =',nhalo,'   (thold =',thold,')'
  
  deallocate(cnt)
  
end subroutine get_nhalo
