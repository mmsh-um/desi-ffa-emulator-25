!> last modified 04-01-2022

subroutine write_output(n,x,y,z,w,xh,yh,zh,wh,halo,halosz,ncut,namout_part,namout_halo,mocknumber)
  
  implicit none

  include 'PRM_string_attributes.f90'

  integer(4) :: n,ncut
  real(8), dimension(n) :: x,y,z,w
  real(8), dimension(n) :: xh,yh,zh,wh
  integer(4), dimension(n) :: halo,halosz
  character(msl) :: namout_part,namout_halo
  integer(4) :: i,j
  character(4) :: mocknumber

  integer(4), parameter :: hist_dim=100
  real(8), parameter :: hist_step=1.d0
  real(8), parameter :: hist0=0.5d0
  real(8), dimension(hist_dim)  :: hist
  
  open(100,file=trim(namout_part))
  do i=1,n
     !write(100,*)x(i),y(i),z(i),halo(i)
     write(100,*)real(x(i)),real(y(i)),real(z(i)),halo(i)
     !write(100,*)halo(i)
  enddo
  close(100)

  open(110,file=trim(namout_halo))
  do i=1,n
     if (halosz(i).ge.ncut) then
        !write(110,*)xh(i),yh(i),zh(i),wh(i),halosz(i),halo(i)
        write(110,*)real(xh(i)),real(yh(i)),real(zh(i)),real(wh(i)),halosz(i),halo(i)
     endif
  enddo
  close(110)

  hist=0
  do i=1,n
     if (halosz(i).gt.0) then
        j=nint((halosz(i)-hist0)/hist_step+0.5d0)
        if (1.le.j.and.j.le.hist_dim) hist(j)=hist(j)+1.d0
     endif
  enddo
  
  open(200,file='out/histogram_m'//trim(mocknumber)//'.dat')
  do i=1,hist_dim
     write(200,*)real(hist0+(i-0.5d0)*hist_step),real(hist(i))
  enddo
  write(200,*)
  write(200,*)real(maxval(halosz)),'max value'
  close(200)
  
end subroutine write_output
