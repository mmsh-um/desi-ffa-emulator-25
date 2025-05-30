subroutine adjust_qref(qref,cnt,beta0,beta1,write_adj_lg,emudir)
  
  implicit none

  include 'PRM_string_attributes.f90'
  include 'PRM_selprop_bins.f90'

  real(8), dimension(nbins_cfc,nbins_tnt) :: qref
  integer(4), dimension(nbins_cfc,nbins_tnt) :: cnt
  real(8) :: beta0,beta1
  logical :: write_adj_lg
  character(msl) :: emudir
  character(4) :: mocknum
  integer(4) :: i,j,k
  real(8) :: norm,cfc_b,tnt_b,f,qadj
  real(8), dimension(nbins_cfc,nbins_tnt) :: dq
  
  qref=beta0*qref
  do j=1,nbins_cfc
     cfc_b=j-1
     do k=1,nbins_tnt
        tnt_b=k-1
        f=1+beta1*cfc_b
        qadj=qref(j,k)*f
        qadj=min(1.d0,max(0.d0,qadj))
        dq(j,k)=qadj-qref(j,k)
     enddo
  enddo  
  norm=sum(dq*cnt)/sum(qref*cnt)+1
  write(*,*)' norm =',norm
  qref=(qref+dq)/norm
  
  if (write_adj_lg) then

     open(543, file=trim(emudir)//'out/qref_adj_m'//trim(mocknum)//'.dat')
     do j=1,nbins_cfc
        cfc_b=j-1
        do k=1,nbins_tnt
           tnt_b=k-1
           write(543,*)cfc_b,tnt_b,qref(j,k)!,cnt(j,k)
        enddo
        write(543,*)
     enddo
     close(543)
  endif
  
end subroutine adjust_qref