!> last modified 09-04-2023

subroutine individual_selfrac(npart,w,msk,selmsk,namout)
  
  implicit none
  
  include 'PRM_string_attributes.f90'
  include 'PRM_selprop_bins.f90'
  
  integer(4) :: npart
  real(8), dimension(npart) :: w
  logical, dimension(npart) :: msk,selmsk
  character(msl) :: namout
  integer(4) :: i,k
  real(8), dimension(nlogbins_ntm) :: qm_log
  integer(4), dimension(nlogbins_ntm) :: cnt_log,cnt_sel_log
  real(8) :: ntm,ntm_b
  
  cnt_log=0
  cnt_sel_log=0
  do i=1,npart
     if (msk(i)) then
        ntm=w(i)
        k=1+nint((log10(ntm)-zerolog_ntm)/logbinsz_ntm)
        if (1.le.k.and.k.le.nlogbins_ntm) then
           if (selmsk(i)) cnt_sel_log(k)=cnt_sel_log(k)+1
           cnt_log(k)=cnt_log(k)+1
        endif
     endif
  enddo
  
  where (cnt_log.gt.0.d0)
     qm_log=dble(cnt_sel_log)/cnt_log
  elsewhere
     qm_log=0.d0
  end where

  open(246,file=trim(namout)//'indselfrac_qm_log.dat')
  do k=1,nlogbins_ntm
     ntm_b=10**(zerolog_ntm+(k-1)*logbinsz_ntm)
     write(246,*)ntm_b,qm_log(k),cnt_sel_log(k),cnt_log(k)
  enddo
  close(246)
  
end subroutine individual_selfrac
