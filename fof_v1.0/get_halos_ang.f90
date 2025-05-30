!> last modified 04-06-2023

subroutine get_halos_ang(n,u,w,msk,theta_max,haloid,halosz,cfc,check_lg, fofdir,mocknumber)
  
  implicit none

  include 'PRM_deg_and_rad.f90'
  include 'PRM_llist.f90'
  
  integer(4) :: n
  real(8), dimension(n) :: w
  real(8), dimension(4,n) :: u
  logical, dimension(n) :: msk
  real(8) :: theta_max
  integer(4), dimension(n) :: haloid,halosz,pchain,cfc
  logical :: check_lg
  integer(4), dimension(n) :: lst
  integer(4), dimension(ngrid,ngrid,ngrid) :: label
  real(8) :: cell
  integer(4) :: il,jl,kl,i1,i2,j1,j2,k1,k2,it,jt,kt,n0
  integer(4) :: ip,jp
  integer(4) :: cnt,cnt_tot
  real(8) :: theta,unisep_max
  real(8) :: cell_ll_x,cell_ll_y,cell_ll_z,x0_ll,y0_ll,z0_ll
  integer(4) :: i,j
  logical, parameter :: read_pchain=.false.
  integer(4), save :: niter=0
  character(3) :: niter_c
  integer(4) :: hey
  character(1024) :: fofdir
  character(4) :: mocknumber
  niter=niter+1
  
  unisep_max=2.d0*sin(0.5d0*deg2rad*theta_max)

  !write(*,*)'size(lst) =',size(lst)
 
  write(*,*)'ngrid      =',ngrid
  write(*,*)'theta_max  =',theta_max  
  write(*,*)'unisep_max =',unisep_max
  
  call llist_auto(n,u(1,:),u(2,:),u(3,:),lst,label,ngrid,cell_ll_x,cell_ll_y,cell_ll_z,x0_ll,y0_ll,z0_ll)
  
  if (read_pchain.eqv..false.) then
     
     pchain=-1
     n0=1

     !$omp parallel do private(i,i1,j1,k1,i2,j2,k2,kt,kl,jt,jl,it,il,j,theta,ip,jp) !!!FIXME!!!
     do i=n0,n
        call advo(i,n,5)
        
        if (msk(i)) then
           i1=nint((u(1,i)-x0_ll-unisep_max)/cell_ll_x+0.5d0)
           j1=nint((u(2,i)-y0_ll-unisep_max)/cell_ll_y+0.5d0)
           k1=nint((u(3,i)-z0_ll-unisep_max)/cell_ll_z+0.5d0)
           i1=min(max(i1,1),ngrid)
           j1=min(max(j1,1),ngrid)
           k1=min(max(k1,1),ngrid)
           i2=nint((u(1,i)-x0_ll+unisep_max)/cell_ll_x+0.5d0)
           j2=nint((u(2,i)-y0_ll+unisep_max)/cell_ll_y+0.5d0)
           k2=nint((u(3,i)-z0_ll+unisep_max)/cell_ll_z+0.5d0)
           i2=min(max(i2,1),ngrid)
           j2=min(max(j2,1),ngrid)
           k2=min(max(k2,1),ngrid)
           
           do kl=k1,k2
              do jl=j1,j2
                 do il=i1,i2
                    j=label(il,jl,kl)
                    do while(j.ne.0.and.i.lt.j)
                       if (msk(j)) then
                          theta=acos(dot_product(u(:3,i),u(:3,j)))
                          theta=rad2deg*theta
                          
                          if (theta.le.theta_max) then
                             
                             ip=i
                             do while(pchain(ip).gt.0)
                                ip=pchain(ip)
                             enddo
                             jp=j
                             do while(pchain(jp).gt.0)
                                jp=pchain(jp)
                             enddo
                             
                             if (pchain(j).lt.0) then
                                pchain(j)=i
                             else
                                if (ip.ne.jp) pchain(max(ip,jp))=min(ip,jp)
                             endif

                             cfc(i)=cfc(i)+1
                             cfc(j)=cfc(j)+1
                             
                          endif
                       endif
                       j=lst(j)
                    enddo
                 enddo
              enddo
           enddo
        endif
     enddo
     !$omp end parallel do

     write(*,*)' minval(cfc,msk), maxval(cfc,msk) =',minval(cfc,msk), maxval(cfc,msk)
     write(*,*)' sum(cfc,msk)/count(msk) =',sum(cfc,msk)/dble(count(msk))
     write(*,*)' count(cfc.eq.0) =',count(cfc.eq.0)
     write(*,*)' count(cfc.gt.0) =',count(cfc.gt.0)
     write(*,*)' count(cfc.lt.0) =',count(cfc.lt.0)
     
     write(*,*)' writing binary file ...'
     call itg2chr(niter,niter_c)
     open(605,file=trim(fofdir)//'out/raw_pchain_'//trim(niter_c)//'_m'//trim(mocknumber)//'.dat',form='unformatted',access='stream')
     write(605)pchain
     close(605)
     write(*,*)' done'
     
  else
     
     write(*,*)' reading binary file ...'
     call itg2chr(niter,niter_c)
     open(605,file=trim(fofdir)//'out/raw_pchain_'//trim(niter_c)//'_m'//trim(mocknumber)//'.dat',form='unformatted',access='stream') 
     read(605)pchain
     close(605)
     write(*,*)' done'

  endif
  
  !haloid=-2
  do i=1,n
     if (msk(i)) then
        ip=i
        do while(pchain(ip).gt.0)
           ip=pchain(ip)
        enddo
        haloid(i)=ip
     endif
  enddo
  write(*,*)' count(haloid.eq.-2) =',count(haloid.eq.-2)
  write(*,*)' count(haloid.gt.0) =',count(haloid.gt.0)
  
  if (check_lg) then
     
     write(*,*)' checking ...'
     
     cnt=0
     cnt_tot=0
     do i=n0,n
        call advo(i,n,5)
        
        if (msk(i)) then
           i1=nint((u(1,i)-x0_ll-unisep_max)/cell_ll_x+0.5d0)
           j1=nint((u(2,i)-y0_ll-unisep_max)/cell_ll_y+0.5d0)
           k1=nint((u(3,i)-z0_ll-unisep_max)/cell_ll_z+0.5d0)
           i1=min(max(i1,1),ngrid)
           j1=min(max(j1,1),ngrid)
           k1=min(max(k1,1),ngrid)
           i2=nint((u(1,i)-x0_ll+unisep_max)/cell_ll_x+0.5d0)
           j2=nint((u(2,i)-y0_ll+unisep_max)/cell_ll_y+0.5d0)
           k2=nint((u(3,i)-z0_ll+unisep_max)/cell_ll_z+0.5d0)
           i2=min(max(i2,1),ngrid)
           j2=min(max(j2,1),ngrid)
           k2=min(max(k2,1),ngrid)
           
           do kl=k1,k2
              do jl=j1,j2
                 do il=i1,i2
                    j=label(il,jl,kl)
                    do while(j.ne.0.and.i.lt.j)
                       if (msk(j)) then
                          theta=acos(dot_product(u(:3,i),u(:3,j)))
                          theta=rad2deg*theta
                          
                          if (theta.le.theta_max) then
                             
                             if (haloid(i).ne.haloid(j)) then
                                cnt=cnt+1
                                !write(*,*)i,j
                                !write(*,*)haloid(i),haloid(j)
                                !read(*,*)
                             endif
                             cnt_tot=cnt_tot+1
                          endif
                       endif
                       j=lst(j)
                    enddo
                 enddo
              enddo
           enddo
        endif
     enddo
     write(*,*)' done'
     write(*,*)' cnt =',cnt
     write(*,*)' cnt_tot =',cnt_tot
     write(*,*)' ~~> ',real(cnt)/cnt_tot*100,'%'
  endif
     
end subroutine get_halos_ang
   
