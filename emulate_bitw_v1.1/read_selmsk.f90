!> last modified 29-01-2023

subroutine read_selmsk(npart,selmsk,namin)
    
  implicit none

  include 'PRM_string_attributes.f90'
  
  integer(4) :: npart
  logical, dimension(npart) :: selmsk
  character(msl) :: namin
  integer(4) :: i
  real(4) :: dum
  integer(4) :: idum
  logical :: ldum
  
  open(10,file=trim(namin),action="read")
  do i=1,npart
     call advo(i,npart,5) 
     read(10,*)dum,dum,dum,dum,idum,ldum,selmsk(i)     
  enddo
  close(10)

end subroutine read_selmsk
