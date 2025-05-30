!> last modified 29-01-2023

subroutine get_selprop(sz,w,selmsk,ngsel,nt)

  implicit none

  integer(4) :: sz,ngsel
  real(8) :: nt
  real(8), dimension(sz) :: w
  logical, dimension(sz) :: selmsk
  integer(4) :: i

  ngsel=count(selmsk)
  nt=sum(w)
  
end subroutine get_selprop
