!> last modified 16-06-2023

subroutine antico(nobj,l,idum,deco)

  implicit none

  include 'PRM_we.f90'

  integer(4) :: nobj,idum 
  logical, dimension(nobj,nbits) :: l
  real(8) :: deco
  integer(4) :: i,j,k
  integer(4), dimension(nbits) :: pc
  integer(4) :: n,navail,mpc,ngsel
  logical, dimension(nbits) :: id_min,lt 
  integer(4) :: ideco,j1,j2
  logical :: bt
  real(4), external :: ran1 
  integer(4), dimension(nbits) :: newidx,avail
  
  where(l(1,:)) 
     pc=1
  elsewhere
     pc=0
  endwhere

  do i=2,nobj
     n=count(l(i,:))
     l(i,:)=.false.
     navail=n
     
     do j=1,nbits
        mpc=minval(pc)
        where(pc.eq.mpc.and..not.l(i,:))
           id_min=.true.
        elsewhere
           id_min=.false.
        endwhere

        ngsel=min(count(id_min),navail)
        lt=.false.
        call ranidx_vetomask(int(nbits),ngsel,lt,idum,id_min)
        where(lt)
           l(i,:)=.true.
           pc=pc+1
        endwhere
        
        navail=navail-count(lt)
        if (count(l(i,:)).ge.n) exit
     enddo
  enddo

  !write(*,*)'pc =',pc
  !write(*,*)'min,maxval(pc) =',minval(pc),maxval(pc)
  !write(*,*)'sum(pc) =',sum(pc),'(vs',count(l),')'


  ideco=min(max(0,nint(deco*nbits)),nbits)
  if (ideco.gt.0) then
     do i=1,nobj
        do k=1,ideco
           j1=1+int(nbits*ran1(idum))
           j2=1+int(nbits*ran1(idum))
           bt=l(i,j1)
           l(i,j1)=l(i,j2)
           l(i,j2)=bt
        enddo
     enddo
  endif

!!$  !> Randomising realisation space
!!$  do i=1,nbits
!!$     avail(i)=i
!!$  enddo
!!$  newidx=0
!!$  k=nbits
!!$  do i=1,nbits
!!$     j=int(k*ran1(idum))+1
!!$     newidx(avail(j))=i
!!$     avail(j:k)=avail(j+1:k+1)
!!$     k=k-1
!!$  enddo
!!$  do i=1,nobj
!!$     do j=1,nbits
!!$        lt(j)=l(i,newidx(j))        
!!$     enddo
!!$     l(i,:)=lt
!!$  enddo
  
end subroutine antico


!!$program test_antico
!!$
!!$  implicit none
!!$
!!$  include 'PRM_we.f90'
!!$
!!$  integer(4), parameter :: nobj=5
!!$  logical, dimension(nobj,nbits) :: l
!!$  integer(4) :: idum,ngsel
!!$  integer(4) :: i,j
!!$  logical, dimension(nbits) :: lt
!!$  real(4), external :: ran1
!!$  real(8) :: s
!!$  real(8) :: deco
!!$
!!$  deco=0.d0
!!$  
!!$  idum=-785768
!!$
!!$  write(*,*)'nbits =',nbits
!!$  write(*,*)'nobj =',nobj
!!$  write(*,*)'deco =',deco
!!$  
!!$  l=.false.
!!$  lt=.true.
!!$  do i=1,nobj
!!$     ngsel=int((nbits+1)*ran1(idum))
!!$     !ngsel=nbits/nobj
!!$     call ranidx_vetomask(int(nbits),ngsel,l(i,:),idum,lt)
!!$     write(*,*)i,ngsel
!!$     !write(*,*)l(i,:)
!!$  enddo
!!$
!!$  s=0.d0
!!$  do i=1,nobj
!!$     do j=i+1,nobj
!!$        !write(*,*)count(l(i,:).and.l(j,:))
!!$        s=s+count(l(i,:).and.l(j,:))
!!$     enddo
!!$  enddo
!!$  write(*,*)'s =',s
!!$  
!!$  call antico(nobj,l,idum,deco)
!!$
!!$  do i=1,nobj
!!$     write(*,*)i,count(l(i,:))
!!$  enddo
!!$  
!!$  s=0.d0
!!$  do i=1,nobj
!!$     do j=i+1,nobj
!!$        !write(*,*)count(l(i,:).and.l(j,:))
!!$        s=s+count(l(i,:).and.l(j,:))
!!$     enddo
!!$  enddo
!!$  write(*,*)'s =',s
!!$  
!!$end program test_antico
!!$
!!$include 'ran1.for'
!!$include 'ranidx_vetomask.f90'
