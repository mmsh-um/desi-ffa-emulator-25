!> last modified 04-06-2023

!> Friends-Of-Friends finder
!>
!> MEANING OF THE VARIABLES USED TO NAVIGATE THE HALOS
!>
!> "haolid" is the identifier assigned to each halo.
!> All galaxy belonging to the same halo share the same haloid.
!> It is choosen as the smallest among the identifiers the galaxy belonging to that halo
!> (but the chains should work even if haloid is generic integer in [1,npart])
!>    haloid = -2 for masked galaxies
!>    
!> "chain_ini" initialises the chain:
!>    chain_ini = -2 for masked galaxies 
!>    chain_ini = the number of object in the halo for the galaxy with the smallest identifier belonging to that halo
!>    chain_ini = 0 for all other galaxies
!>
!> "chain" links the galaxy inside a given halo, from the one with smallest identifier to the one with the largest:
!>    chain = -2 for masked galaxies
!>    chain = -1 for the last galaxy in the chain
!>    chain = identifier of the next galaxy in the chain for all other galaxies
!>
!> "nhalo" is the total number of halos (with number of galaxies >= ncut) 

program Friend_Of_Friend

  implicit none
  
  !include 'PRM_fof_per_2d.f90' 
  include 'PRM_llist.f90'
  include 'PRM_string_attributes.f90'
  
  character(msl) :: namin,namout_part,namout_halo, fof_dir
  integer(4) :: npart,nhalos,ncut
  integer(4) :: tartype0,nsplit
  character :: galcap
  logical :: just_cfc_lg
  real(8) :: linklen
  real(8), dimension(:), allocatable :: x,y,z,w
  logical, dimension(:), allocatable :: msk
  integer(4), dimension(:), allocatable :: haloid,chain_ini,chain
  character(8) :: sfx
  character(1) :: dum
  character(msl) :: namini
  real(8) :: box
  real(8), dimension(:), allocatable :: wh
  real(8), dimension(:,:), allocatable :: u
  real(8), dimension(:), allocatable :: ra,dec,rshift
  real(8) :: x0,y0,z0
  real(8), dimension(:), allocatable :: rah,dech,rshifth
  integer(4), dimension(:), allocatable :: haloidh,sz
  integer(4) :: i,j,k
  real(8) :: d
  integer(4) :: nhalo
  real(8), dimension(:), allocatable :: w_tup
  real(8), dimension(:,:), allocatable :: u_tup
  logical, dimension(:), allocatable :: msk_tup
  real(8), dimension(4) :: uh
  integer(4) :: ih,idum
  logical, dimension(:), allocatable :: msk_t
  integer(4), dimension(:), allocatable :: group_split 
  !integer(4), dimension(:), allocatable :: duplicates
  integer(4), dimension(:), allocatable :: cfc
  character(4) :: mocknumber
  
  call write_time_and_date()

  !namini='INI_corfu.txt'
  call get_command_argument(1,namini)
  write(*,*)'namini = ',trim(namini)
  open(1000,file=trim(namini),action="read")
  read(1000,*)dum,namin
  read(1000,*)dum,namout_part
  read(1000,*)dum,namout_halo
  read(1000,*)dum,npart
  read(1000,*)dum,linklen
  read(1000,*)dum,ncut
  read(1000,*)dum,tartype0
  read(1000,*)dum,galcap
  read(1000,*)dum,nsplit
  read(1000,*)dum,just_cfc_lg
  read(1000,*)dum,fof_dir
  read(1000,*)dum,mocknumber
  close(1000)
  
  write(*,*)'fof_dir     = ',trim(fof_dir)
  write(*,*)'namin       = ',trim(namin)
  write(*,*)'namout_part = ',trim(namout_part)
  write(*,*)'namout_halo = ',trim(namout_halo)
  write(*,*)'npart       =',npart
  write(*,*)'linklen     =',linklen
  write(*,*)'ncut        =',ncut
  write(*,*)'tartype0    =',tartype0
  write(*,*)'galcap      = ',galcap
  write(*,*)'nsplit      =',nsplit
  write(*,*)'just_cfc_lg =',just_cfc_lg
  write(*,*)'mocknum     =',mocknumber
  
  allocate(x(npart),y(npart),z(npart),w(npart))
  allocate(msk(npart))
  allocate(haloid(npart),chain_ini(npart),chain(npart))
  allocate(ra(npart),dec(npart),rshift(npart))
  allocate(u(4,npart))
  allocate(cfc(npart))
  
  write(*,*)'reading galaxy catalogue ...'
  w=1.d0
  msk=.true.
  call read_sample_lightcone(npart,ra,dec,rshift,w,msk,tartype0,galcap,namin)
  write(*,*)'done'
  write(*,*)'count(msk) =',count(msk)
  
  write(*,*)'converting galaxies to cartesian ...'
  call get_cartco(npart,ra,dec,rshift,x,y,z,.false.)
  write(*,*)'done'
  
  call unisphe(npart,x,y,z,u)
  
  deallocate(x,y,z)

  write(*,*)'finding halos ...'
  idum=-3456742
  !call dilu(npart,msk,0.5d0,idum)
  allocate(group_split(npart),msk_t(npart))
  call split(npart,msk,nsplit,idum,group_split)
  haloid=-2
  where(msk)
     cfc=0
  elsewhere
     cfc=-2
  endwhere
  do i=1,nsplit
     msk_t=msk
     where (group_split.ne.i)
        msk_t=.false.
     end where
     write(*,*)'count(msk_t) =',count(msk_t)
     if (just_cfc_lg) then
        write(*,*)' (CFC)'
        do j=1,npart
           if (msk_t(j)) haloid(j)=j
        enddo
        chain_ini=-3
        call get_cfc_ang(npart,u,w,msk_t,linklen,cfc)
     else
        write(*,*)' (FOF)'
        call get_halos_ang(npart,u,w,msk_t,linklen,haloid,chain_ini,cfc,.false., fof_dir,mocknumber)
     endif
  enddo
  deallocate(group_split,msk_t)
  write(*,*)'done'
  
  write(*,*)'writing haloid for each galaxy ...'
  call write_haloid_lightcone(npart,ra,dec,rshift,w,msk,haloid,cfc,namout_part)
  write(*,*)'done'
  
  deallocate(ra,dec,rshift)

  if (.not.just_cfc_lg) then
     write(*,*)'computing particle chain ...'
     call get_chain(npart,haloid,msk,chain,chain_ini)
     write(*,*)'done'
     
     write(*,*)'computing number of halos ...'
     call get_nhalo(npart,msk,haloid,ncut,nhalo)
     write(*,*)'done'
     
!!$  allocate(duplicates(npart))
!!$  duplicates=0
!!$  do i=1,npart
!!$     if (chain_ini(i).ge.ncut) then
!!$        j=i
!!$        do while(j.gt.0)
!!$           duplicates(j)=duplicates(j)+1
!!$           j=chain(j)
!!$        enddo
!!$     endif
!!$  enddo
!!$  write(*,*)'count(duplicates.gt.1)',count(duplicates.gt.1)
!!$  deallocate(duplicates)
  
     allocate(wh(nhalo))
     allocate(rah(nhalo),dech(nhalo),rshifth(nhalo))
     allocate(sz(nhalo),haloidh(nhalo))
     
     write(*,*)'computing halo properties ...'
     wh=0.d0
     sz=0
     ih=1
     do i=1,npart
        call advo(i,npart,5)
        if (chain_ini(i).ge.ncut) then
           allocate(u_tup(4,chain_ini(i)))  
           allocate(w_tup(chain_ini(i)))
           allocate(msk_tup(chain_ini(i)))
           j=i
           k=1
           do while(j.gt.0)
              u_tup(:,k)=u(:,j)
              w_tup(k)=w(j)
              msk_tup(k)=msk(j)
              j=chain(j)
              k=k+1
           enddo
           haloidh(ih)=haloid(i)
           wh(ih)=sum(w_tup,msk_tup)
           sz(ih)=chain_ini(i)
           call get_halo_centers_ang(sz(ih),u_tup,w_tup,msk_tup,uh)
           call polaco(uh(4)*uh(1),uh(4)*uh(2),uh(4)*uh(3),rah(ih),dech(ih),rshifth(ih))
           deallocate(u_tup)
           deallocate(w_tup)
           deallocate(msk_tup)
           ih=ih+1
        endif
     enddo
     write(*,*)'done'
     
     write(*,*)'writing halo properties ...'
     call write_halo_properties_lightcone(nhalo,rah,dech,rshifth,wh,sz,haloidh,.true.,namout_halo, fof_dir,mocknumber)
     write(*,*)'done'
     deallocate(wh)
     deallocate(rah,dech,rshifth)
     deallocate(sz,haloidh)
  endif
     
  deallocate(w)
  deallocate(msk)  
  deallocate(haloid,chain_ini)
  deallocate(u)
  deallocate(cfc)
  
  call write_time_and_date()

end program Friend_Of_Friend

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> ROUTINES

include 'advo.f90'
!include 'advs.f90'
include 'cartco.f90'
include 'dilu.f90'
include 'get_cartco.f90'
include 'get_cfc_ang.f90'
include 'get_chain.f90'
!include 'get_grhalos_ang.f90'
include 'get_halo_centers_ang.f90'
include 'get_halos_ang.f90'
include 'get_nhalo.f90'
include 'hubb.f90'
include 'itg2chr.f90'
include 'llist_auto.f90'
include 'polaco.f90'
include 'qgaus.for'
include 'ran1.for'
include 'read_sample_lightcone.f90'
include 'rshift2dist_integrand.f90'
include 'split.f90'
include 'unisphe.f90'
include 'write_halo_properties_lightcone.f90'
include 'write_haloid_lightcone.f90'
include 'write_time_and_date.f90'




