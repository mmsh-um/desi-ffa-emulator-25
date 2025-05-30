!> lin binning
  integer(4), parameter :: nbins_sz=20
  real(8), parameter :: binsz_sz=30
  integer(4), parameter :: nbins_ntm=20
  real(8), parameter :: binsz_ntm=0.3

 !>log binning
  integer(4), parameter :: nlogbins_sz=24
  real(8), parameter    :: logbinsz_sz=0.15
  real(8), parameter    :: zerolog_sz=0.d0
  integer(4), parameter :: nlogbins_ntm=12
  real(8), parameter    :: logbinsz_ntm=0.14
  real(8), parameter    :: zerolog_ntm=-0.5d0

!!$!>log binning
!!$  integer(4), parameter :: nlogbins_sz=10
!!$  real(8), parameter    :: logbinsz_sz=0.3
!!$  real(8), parameter    :: zerolog_sz=0.d0
!!$  integer(4), parameter :: nlogbins_ntm=10
!!$  real(8), parameter    :: logbinsz_ntm=0.15
!!$  real(8), parameter    :: zerolog_ntm=-0.5d0

  !>tnt-cfc binning
  integer(4), parameter :: nbins_tnt=10
  integer(4), parameter :: nbins_cfc=100
