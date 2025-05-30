!> last modified 09-04-2023

subroutine read_qref_individual(qref,namin)
  
  implicit none

  include 'PRM_selprop_bins.f90'
  include 'PRM_string_attributes.f90'
  
  real(8), dimension(nlogbins_ntm) :: qref
  character(msl) :: namin
  integer(4) :: j
  real(8) :: ntm
  
  open(433,file=trim(namin),action="read")
  do j=1,nlogbins_ntm
     read(433,*)ntm,qref(j)
  enddo
  close(433)
  
end subroutine read_qref_individual
