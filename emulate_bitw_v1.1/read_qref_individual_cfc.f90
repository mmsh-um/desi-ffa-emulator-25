!> last modified 22-06-2023

subroutine read_qref_individual_cfc(qref,cnt,namin)
  
  implicit none

  include 'PRM_selprop_bins.f90'
  include 'PRM_string_attributes.f90'
  
  real(8), dimension(nbins_cfc,nbins_tnt) :: qref
  integer(4), dimension(nbins_cfc,nbins_tnt) :: cnt
  character(msl) :: namin
  integer(4) :: j,k
  real(8) :: cfc,tnt
  integer(4) :: cnt_sel
  
  open(443,file=trim(namin),action="read")
  do j=1,nbins_cfc
     do k=1,nbins_tnt 
        read(443,*)cfc,tnt,qref(j,k),cnt_sel,cnt(j,k)
     enddo
  enddo
  close(443)
  
end subroutine read_qref_individual_cfc
