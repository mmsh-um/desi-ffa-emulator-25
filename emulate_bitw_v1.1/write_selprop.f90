!> last modified 29-01-2023

subroutine write_selprop(nhalo,sz,ngsel,nt,linbins_lg,logbins_lg,namout)
  
  implicit none
  
  include 'PRM_string_attributes.f90'
  include 'PRM_selprop_bins.f90'
  
  integer(4) :: nhalo
  integer(4), dimension(nhalo) :: sz,ngsel 
  real(8), dimension(nhalo) :: nt
  character(msl) :: namout
  logical :: linbins_lg,logbins_lg
  integer(4) :: i,j,k
  real(8) :: ntm,q,sz_b,ntm_b
  real(8), dimension(nbins_sz,nbins_ntm) :: qm
  integer(4), dimension(nbins_sz,nbins_ntm) :: cnt
  real(8), dimension(nlogbins_sz,nlogbins_ntm) :: qm_log
  integer(4), dimension(nlogbins_sz,nlogbins_ntm) :: cnt_log

!!$  do j=1,nlogbins_sz
!!$     sz_b=10**(zerolog_sz+(j-1)*logbinsz_sz)
!!$     write(*,*)sz_b
!!$  enddo
!!$  do k=1,nlogbins_ntm
!!$     ntm_b=10**(zerolog_ntm+(k-1)*logbinsz_ntm)
!!$     write(*,*)ntm_b
!!$  enddo
!!$  read(*,*)
  
  !open(220,file=trim(namout))
  open(220,file=trim(namout)//'selprop.dat')
  do i=1,nhalo
     write(220,*)sz(i),ngsel(i),nt(i)
  enddo
  close(220)

  if (linbins_lg) then   
     qm=0.d0
     cnt=0
     do i=1,nhalo
        j=nint(sz(i)/binsz_sz-0.5)+1
        ntm=nt(i)/sz(i)
        k=nint(ntm/binsz_ntm-0.5)+1
        q=dble(ngsel(i))/sz(i)
        if (1.le.j.and.j.le.nbins_sz.and.1.le.k.and.k.le.nbins_ntm) then
           qm(j,k)=qm(j,k)+q
           cnt(j,k)=cnt(j,k)+1
        endif
     enddo
     
     do j=1,nbins_sz
        do k=1,nbins_ntm
           if (cnt(j,k).gt.0) qm(j,k)=qm(j,k)/cnt(j,k)
        enddo
     enddo
     
     !open(230,file='out/qm_lin.dat')
     open(230,file=trim(namout)//'qm_lin.dat')
     do j=1,nbins_sz
        sz_b=(j-0.5)*binsz_sz
        do k=1,nbins_ntm
           ntm_b=(k-0.5)*binsz_ntm
           write(230,*)sz_b,ntm_b,qm(j,k)
        enddo
        write(230,*)
     enddo
     close(230)
  endif
  
  if (logbins_lg) then    
     qm_log=0.d0
     cnt_log=0
     do i=1,nhalo
        j=1+nint((log10(dble(sz(i)))-zerolog_sz)/logbinsz_sz)
        ntm=nt(i)/sz(i)
        k=1+nint((log10(ntm)-zerolog_ntm)/logbinsz_ntm)
        q=dble(ngsel(i))/sz(i)
        if (1.le.j.and.j.le.nlogbins_sz.and.1.le.k.and.k.le.nlogbins_ntm) then
           qm_log(j,k)=qm_log(j,k)+q
           cnt_log(j,k)=cnt_log(j,k)+1
        endif
     enddo
     
     do j=1,nlogbins_sz
        do k=1,nlogbins_ntm
           if (cnt_log(j,k).gt.0) qm_log(j,k)=qm_log(j,k)/cnt_log(j,k)
        enddo
     enddo
     
     !open(240,file='out/qm_log.dat')
     open(240,file=trim(namout)//'qm_log.dat')
     do j=1,nlogbins_sz
        sz_b=10**(zerolog_sz+(j-1)*logbinsz_sz)
        do k=1,nlogbins_ntm
           ntm_b=10**(zerolog_ntm+(k-1)*logbinsz_ntm)
           write(240,*)sz_b,ntm_b,qm_log(j,k)
        enddo
        write(240,*)
     enddo
     close(240)
  endif
  
end subroutine write_selprop
