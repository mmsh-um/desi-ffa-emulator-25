!> last modified 30-01-2023

subroutine write_halo_properties_lightcone(nhalo,rah,dech,rshifth,wh,sz,haloidh,hist_lg,namout_halo, fofdir,mocknumber)
  
  implicit none

  include 'PRM_string_attributes.f90'

  integer(4) :: nhalo
  real(8), dimension(nhalo) :: rah,dech,rshifth,wh
  integer(4), dimension(nhalo) :: haloidh,sz
  logical :: hist_lg
  character(msl) :: namout_halo
  integer(4) :: i,j

  integer(4), parameter :: hist_dim=100
  real(8), parameter :: hist_step=1.d0
  real(8), parameter :: hist0=0.5d0
  real(8), dimension(hist_dim)  :: hist
  character(1024) :: fofdir
  character(4) :: mocknumber

  open(110,file=trim(namout_halo))
  do i=1,nhalo
     !write(110,*)rah(i),dech(i),rshifth(i),wh(i),sz(i),haloidh(i)
     write(110,*)real(rah(i)),real(dech(i)),real(rshifth(i)),real(wh(i)),sz(i),haloidh(i)
  enddo
  close(110)

  if (hist_lg) then
     hist=0
     do i=1,nhalo
        if (sz(i).gt.0) then
           j=nint((sz(i)-hist0)/hist_step+0.5d0)
           if (1.le.j.and.j.le.hist_dim) hist(j)=hist(j)+1.d0
        endif
     enddo
     
     open(200,file=trim(fofdir)//'out/histogram_m'//trim(mocknumber)//'.dat')
     do i=1,hist_dim
        write(200,*)real(hist0+(i-0.5d0)*hist_step),real(hist(i))
     enddo
     write(200,*)
     write(200,*)real(maxval(sz)),'max value'
     close(200)
  endif
     
end subroutine write_halo_properties_lightcone
