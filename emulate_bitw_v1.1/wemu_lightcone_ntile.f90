!> last modified 06-04-2023

subroutine wemu_lightcone_ntile(nobj,w,l,idum,qref,selmsk,mu_sel,sg_sel,frac_nt0,true_ngsel_lg)

  implicit none
  
  include 'PRM_we.f90'
  include 'PRM_selprop_bins.f90'
  
  integer(4) :: nobj,idum
  real(8), dimension(nlogbins_sz,nlogbins_ntm) :: qref
  logical, dimension(nobj) :: selmsk
  real(8) :: mu_sel,sg_sel,frac_nt0
  real(8), dimension(nobj) :: w
  logical, dimension(nobj,nbits) :: l
  logical :: true_ngsel_lg
  integer(4) :: ngsel
  real(8) :: sz,ntm,log10sz,log10ntm,q,qfuzz
  real(8), external :: bilin_ipol,sigmoid
  real(4), external :: ran1,gasdev
  integer(4) :: j,k
  logical, dimension(nobj) :: msk_wth

  msk_wth=.true.
  
  if (true_ngsel_lg) then

     ngsel=count(selmsk)

  else

     !> frac_nt0 >= 1   nothing happens (i.e. all particles with w < 1 are incloded)
     !> frac_nt0 <  1   only some of the particles with w < 1 are incloded, with probability frac_nt0 (w doesn't change)
     
     if (frac_nt0.lt.1.0d0) then
        do j=1,nobj 
           if (w(j).lt.1.d0) then
              if (ran1(idum).gt.frac_nt0) msk_wth(j)=.false.
           endif
        enddo
     endif
     
     !sz=dble(nobj)
     !ntm=sum(w)/sz
     sz=dble(count(msk_wth))
     ntm=sum(w,msk_wth)/sz
     
     log10sz=log10(sz)
     log10ntm=log10(ntm)
     q=bilin_ipol(log10sz,log10ntm,qref,nlogbins_sz,logbinsz_sz,zerolog_sz,nlogbins_ntm,logbinsz_ntm,zerolog_ntm)
     
     qfuzz=mu_sel+sg_sel*gasdev(idum)
     
     !qfuzz=qfuzz+sigmoid(sz-2)*(1-qfuzz)
     !qfuzz=qfuzz+(1-sigmoid(sz-5))*(1-qfuzz)
     
     !q=q+qfuzz
     q=q*qfuzz
     !q=dble(count(selmsk))/nobj     
     !q=max(0.d0,min(1.d0,q)) 

     ngsel=nint(q*sz)
     
  endif
  
  ngsel=max(0,min(ngsel,count(msk_wth)))
  do k=1,nbits
     !call ranidx(nobj,ngsel,l(:,k),idum)
     call ranidx_vetomask(nobj,ngsel,l(:,k),idum,msk_wth)
  enddo
  
end subroutine wemu_lightcone_ntile
