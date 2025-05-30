!> last modified 20-05-2022

subroutine we2wiip(n,we,w,eff_lg)
  
  implicit none
  
  include 'PRM_we.f90'

  integer(4) :: n
  integer(itype), dimension(n,nwe) :: we
  real(8), dimension(n) :: w
  logical :: eff_lg
  integer(4) :: i,m,nsel
  logical :: first

  !we=2147483647

  first=.true.
  do i=1,n
     !if (mod(i,n/10).eq.0) write(*,*)' ',nint(100*real(i)/n),'%'
     !call advo(i,n,5)
     nsel=0
     do m=1,nwe
        nsel=nsel+popcnt(we(i,m))
     enddo
     if (eff_lg) then 
        w(i)=dble(nbits+1)/(nsel+1)
     else
        if (nsel.gt.0) then
           w(i)=dble(nbits)/nsel
        else
           w(i)=w_nsel0
        endif
     endif
     if (nsel.eq.0.and.first) then
        write(*,*)'WARNING: nsel=0 for at least 1 particle'
        first=.false.
     endif
  enddo
  
end subroutine we2wiip
