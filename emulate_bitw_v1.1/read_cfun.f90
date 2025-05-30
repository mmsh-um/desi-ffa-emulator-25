!> last modified 26-01-2022

subroutine read_cfun(nbins_m,binsz_m,nbins_s,binsz_s,cfun,namin)
  
  implicit none

  include 'PRM_we.f90'
  include 'PRM_string_attributes.f90'

  integer(4) :: nbins_m,nbins_s
  real(8) :: binsz_m,binsz_s
  real(8), dimension(nbins_m,nbins_s) :: cfun
  character(msl) :: namin
  integer(4) :: i,j
  real(8) :: m 
  
  open(200,file=trim(namin),action="read")
  do i=1,nbins_m
     m=(i-0.5d0)*binsz_m
     read(200,*)m,(cfun(i,j),j=1,nbins_s)
  enddo
  close(200)
  
end subroutine read_cfun
