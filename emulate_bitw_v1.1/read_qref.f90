!> last modified 01-02-2023

subroutine read_qref(qref,namin)
  
  implicit none

  include 'PRM_selprop_bins.f90'
  include 'PRM_string_attributes.f90'
  
  real(8), dimension(nlogbins_sz,nlogbins_ntm) :: qref
  character(msl) :: namin
  integer(4) :: i,j
  real(8) :: sz,ntm
  
  open(430,file=trim(namin),action="read")
  do i=1,nlogbins_sz
     do j=1,nlogbins_ntm
        read(430,*)sz,ntm,qref(i,j)
     enddo
  enddo
  close(430)
  
end subroutine read_qref
