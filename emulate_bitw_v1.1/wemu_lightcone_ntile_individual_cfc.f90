!> last modified 04-06-2023

subroutine wemu_lightcone_ntile_individual_cfc(npart,w,cfc,l,idum,qref,msk,mu_sel,sg_sel,frac_nt0)

  implicit none
  
  include 'PRM_we.f90'
  include 'PRM_selprop_bins.f90'
  
  integer(4) :: npart,idum
  real(8), dimension(nbins_cfc,nbins_tnt) :: qref
  real(8), dimension(npart) :: w
  integer(4), dimension(npart) :: cfc
  logical, dimension(npart) :: msk
  real(8) :: mu_sel,sg_sel,frac_nt0
  logical, dimension(npart,nbits) :: l
  real(8) :: q,qfuzz
  real(8), external :: sigmoid
  real(4), external :: ran1,gasdev
  integer(4) :: i,j,k,m

  !l=.false.
  do i=1,npart
     call advo(i,npart,5)
     if (msk(i)) then
        j=cfc(i)+1
        k=nint(w(i))+1
        j=(min(nbins_cfc,max(1,j)))
        k=(min(nbins_tnt,max(1,k)))
        q=qref(j,k)

        !if (j.gt.5) q=q*1.5
        
        do m=1,nbits
           if (ran1(idum).le.q) l(i,m)=.true.
        enddo
     endif
  enddo
        
end subroutine wemu_lightcone_ntile_individual_cfc
