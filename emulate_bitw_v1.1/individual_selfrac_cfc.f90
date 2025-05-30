!> last modified 04-06-2023

subroutine individual_selfrac_cfc(npart,tnt,cfc,msk,selmsk,namout)
 
  implicit none
  
  include 'PRM_string_attributes.f90'
  include 'PRM_selprop_bins.f90'
  
  integer(4) :: npart
  real(8), dimension(npart) :: tnt
  integer(4), dimension(npart) :: cfc 
  logical, dimension(npart) :: msk,selmsk
  character(msl) :: namout
  integer(4) :: i,j,k
  integer(4), dimension(nbins_cfc,nbins_tnt) :: cnt,cnt_sel
  real(8), dimension(nbins_cfc,nbins_tnt) :: qm 
  real(8) :: cfc_b,tnt_b
  
  cnt=0
  cnt_sel=0
  do i=1,npart
     if (msk(i)) then
        j=cfc(i)+1
        k=nint(tnt(i))+1
        j=(min(nbins_cfc,max(1,j)))
        k=(min(nbins_tnt,max(1,k)))
        if (selmsk(i)) cnt_sel(j,k)=cnt_sel(j,k)+1
        cnt(j,k)=cnt(j,k)+1
     endif
  enddo

  where (cnt.gt.0)
     qm=dble(cnt_sel)/cnt
  elsewhere
     qm=0.d0
  end where
  
  open(256,file=trim(namout)//'indselfrac_cfc_qm.dat')
  do j=1,nbins_cfc
     cfc_b=j-1
     do k=1,nbins_tnt
        tnt_b=k-1
        write(256,*)cfc_b,tnt_b,qm(j,k),cnt_sel(j,k),cnt(j,k)
     enddo
     write(256,*)
  enddo
  close(256)

  write(*,*)' sum(cnt_sel) =',sum(cnt_sel)
  write(*,*)' sum(cnt) =',sum(cnt) 
  write(*,*)' sum(qm) =',sum(qm)
  write(*,*)' count(cnt.gt.0) =',count(cnt.gt.0),'(of',nbins_cfc*nbins_tnt,')'
  write(*,*)' sum(qm,cnt.gt.0)/count(cnt.gt.0) =',sum(qm,cnt.gt.0)/count(cnt.gt.0)

end subroutine individual_selfrac_cfc
