!> last modified 09-04-2023

subroutine wemu_lightcone_ntile_individual(npart,w,l,idum,qref,msk,mu_sel,sg_sel,frac_nt0)

  implicit none
  
  include 'PRM_we.f90'
  include 'PRM_selprop_bins.f90'
  
  integer(4) :: npart,idum
  real(8), dimension(nlogbins_ntm) :: qref
  real(8), dimension(npart) :: w
  logical, dimension(npart) :: msk
  real(8) :: mu_sel,sg_sel,frac_nt0
  logical, dimension(npart,nbits) :: l
  real(8) :: q,qfuzz
  real(8), external :: ipol,sigmoid
  real(4), external :: ran1,gasdev
  integer(4) :: i,j,k
  real(8), dimension(nlogbins_ntm) :: ntm_b

  do k=1,nlogbins_ntm
     ntm_b(k)=10**(zerolog_ntm+(k-1)*logbinsz_ntm)
  enddo
  
  !l=.false.
  do i=1,npart
     call advo(i,npart,5)
     if (msk(i)) then
        q=ipol(w(i),nlogbins_ntm,ntm_b,qref)
        do j=1,nbits
           if (ran1(idum).le.q) l(i,j)=.true.
        enddo
     endif
  enddo
        
end subroutine wemu_lightcone_ntile_individual
