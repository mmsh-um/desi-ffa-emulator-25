!> last modified 29-01-2023

subroutine read_sample_lightcone(n,ra,dec,rshift,halo,w,msk,cfc,namin)
    
  implicit none

  include 'PRM_string_attributes.f90'
  
  integer(4) :: n
  real(8), dimension(n) :: ra,dec,rshift,w
  integer(4), dimension(n) :: cfc
  logical, dimension(n) :: msk
  integer(4), dimension(n) :: halo
  character(msl) :: namin
  integer(4) :: i
  real(4) :: dum

  logical, parameter :: cut_lg=.false.
  !real(8), parameter :: ra0=-100.d0, ra1=400.d0
  real(8), parameter :: ra0=150.d0, ra1=250.d0
  real(8), parameter :: dec0=-100.d0, dec1=100.d0
  real(8), parameter :: z0=-10.8d0, z1=10.d0
  !real(8), parameter :: z0=0.8d0, z1=1.d0
  
  open(10,file=trim(namin),action="read")
  do i=1,n
     call advo(i,n,5)
     read(10,*)ra(i),dec(i),rshift(i),halo(i),w(i),msk(i),cfc(i)
     !read(10,*)dum,dum,dum,halo(i)
  enddo
  close(10)

  if (cut_lg) then
     write(*,*)' applying RA-dec-z cut..' 
     write(*,*)'  ra0, ra1   =',ra0,ra1
     write(*,*)'  dec0, dec1 =',dec0,dec1
     write(*,*)'  z0, z1     =',z0,z1
     do i=1,n
        if (ra(i).lt.ra0) msk(i)=.false.
        if (ra(i).gt.ra1) msk(i)=.false.
        if (dec(i).lt.dec0) msk(i)=.false.
        if (dec(i).gt.dec1) msk(i)=.false.
        if (rshift(i).lt.z0) msk(i)=.false.
        if (rshift(i).gt.z1) msk(i)=.false.
     enddo
     write(*,*)' done'
  endif
  
end subroutine read_sample_lightcone
