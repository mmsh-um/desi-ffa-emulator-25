!> last modified 11-06-2023

subroutine read_sample_lightcone(n,ra,dec,rshift,w,msk,tartype0,galcap,namin)
  
  implicit none

  include 'PRM_string_attributes.f90'
  
  integer(4) :: n,tartype0
  real(8), dimension(n) :: ra,dec,rshift,w
  logical, dimension(n) :: msk 
  integer(4) :: i
  character :: galcap 
  character(msl) :: namin
  real(4) :: dum
  integer(4) :: tartype
  logical :: ldum
  integer(4) :: tnt 
  character :: gcap
  
  !msk=.true.

  !open(10,file='in/sample.dat')
  open(10,file=trim(namin),action="read")
  do i=1,n
     !call advs(i,n,5)
     call advo(i,n,5)
     !read(10,*)ra(i),dec(i),rshift(i),dum,tartype,ldum,w(i)
     read(10,*)ra(i),dec(i),rshift(i),dum,tartype,msk(i),ldum,w(i),gcap
     if (tartype.ne.tartype0) msk(i)=.false.    !> 34~~>ELG; 1~~>LRG; 4~~>QSO
     if ((galcap.eq.'N'.or.galcap.eq.'S').and.gcap.ne.galcap) msk(i)=.false.   !> N~~>north; S~~>south; B~~>both
     !if (w(i).le.0) msk(i)=.false.
  enddo
  close(10)

end subroutine read_sample_lightcone
