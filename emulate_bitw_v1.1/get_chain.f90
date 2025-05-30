!> last modified 31-01-2023

subroutine get_chain(npart,haloid,msk,chain,chain_ini)

  implicit none

  integer(4) :: npart
  integer(4), dimension(npart) :: haloid,chain,chain_ini
  logical, dimension(npart) :: msk
  integer(4), dimension(:), allocatable :: last
  integer(4) :: i,j
  integer(4) :: cnt
  logical, dimension(:), allocatable :: check
  integer(4) :: cnt_warn_haloid
  
  allocate(last(npart))
  allocate(check(npart))
  
  chain=-2
  last=-1
  cnt=0
  check=.false.
  do i=npart,1,-1
     if (msk(i)) then
        chain(i)=last(haloid(i))
        last(haloid(i))=i
        if (haloid(i).eq.i) cnt=cnt+1
        check(haloid(i))=.true.
     endif
  enddo
  write(*,*)' cnt =',cnt
  write(*,*)' count(check) =',count(check)
  write(*,*)' count(chain.eq.-2) =',count(chain.eq.-2)
  write(*,*)' count(chain.eq.-1) =',count(chain.eq.-1)
  
  deallocate(last)
  deallocate(check)

  where(msk)
     chain_ini=1
  elsewhere
     chain_ini=-2
  endwhere
  do i=1,npart
     if (chain_ini(i).gt.0) then
        !write(*,*)'i =',i
        j=i
        do while(j.gt.0)
           !write(*,*)j,haloid(j)
           chain_ini(j)=0
           j=chain(j)
           chain_ini(i)=chain_ini(i)+1
        enddo
        !write(*,*)'  ~~>',j,chain_ini(i)
        !read(*,*)
     endif
  enddo
  write(*,*)' count(chain_ini.eq.-2) = ',count(chain_ini.eq.-2)
  write(*,*)' count(chain_ini.eq.0) = ',count(chain_ini.eq.0)
  write(*,*)' count(chain_ini.gt.0) = ',count(chain_ini.gt.0)
  write(*,*)' sum(chain_ini,chain_ini.gt.0) =',sum(chain_ini,chain_ini.gt.0)
  write(*,*)' maxval(chain_ini) =',maxval(chain_ini)

  cnt_warn_haloid=0
  do i=1,npart
     if (chain_ini(i).gt.0) then
        if (haloid(i).ne.i) cnt_warn_haloid=cnt_warn_haloid+1
     endif
  enddo
  if (cnt_warn_haloid.gt.0) then
     write(*,*)' WARNING, haloid is not the smallest galaxy id for at least one halo'
     write(*,*)'  (cnt_warn_haloid =',cnt_warn_haloid,')'
  endif
  
end subroutine get_chain
