!> last modified 25-10-2016

  integer(2), parameter :: itype=4
  integer(2), parameter :: nwe=9
  integer(2), parameter :: nbits=(itype*8-1)*nwe
  !integer(2), parameter :: nbits=64
  integer(itype), parameter :: we_fix=2147483647
  real(8), parameter :: w_nsel0=0.d0
  !real(8), parameter :: w_nsel0=dble(nbits)
