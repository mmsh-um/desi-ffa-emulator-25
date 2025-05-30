!> last modified 25-05-2019

subroutine llist_auto(n,x,y,z,lst,label,ngrid,cell_x,cell_y,cell_z,x0,y0,z0)
      
  implicit none
  
  integer(4) ::  n,ngrid
  real(8), dimension(n) :: x,y,z
  integer(4), dimension(n) :: lst
  integer(4), dimension(ngrid,ngrid,ngrid) :: label
  real(8) :: cell_x,cell_y,cell_z
  integer(4) :: i,j,k,idx,part_out
  real(8) :: x0,y0,z0,box_x,box_y,box_z
  !logical :: first
  logical, parameter :: verbose=.false.
  
  x0=minval(x)
  box_x=maxval(x)-x0
  y0=minval(y)
  box_y=maxval(y)-y0
  z0=minval(z)
  box_z=maxval(z)-z0
  cell_x=box_x/ngrid
  cell_y=box_y/ngrid
  cell_z=box_z/ngrid

  if (verbose) then
     write(*,*)'x0     =',x0
     write(*,*)'y0     =',y0
     write(*,*)'z0     =',z0
     write(*,*)'box_x  =',box_x
     write(*,*)'box_y  =',box_y
     write(*,*)'box_z  =',box_z
     write(*,*)'cell_x =',cell_x
     write(*,*)'cell_y =',cell_y
     write(*,*)'cell_z =',cell_z
!!$     write(*,*)minloc(x),maxloc(x)
!!$     write(*,*)minloc(y),maxloc(y)
!!$     write(*,*)minloc(z),maxloc(z)
  endif
     
  part_out=0
  lst=-1
  label=0     
  !first=.true.
  do idx=1,n
     !i=1+int((x(idx)-x0)/cell)
     !j=1+int((y(idx)-y0)/cell)
     !k=1+int((z(idx)-z0)/cell)
     i=nint((x(idx)-x0)/cell_x+0.5d0)
     j=nint((y(idx)-y0)/cell_y+0.5d0)
     k=nint((z(idx)-z0)/cell_z+0.5d0)
!!$     if ((i.lt.1.or.j.lt.1.or.k.lt.1.or.i.gt.ngrid_x.or.j.gt.ngrid_y.or.k.gt.ngrid_z).and.first) then
!!$        write(*,*)'WARNING, particle out of grid'
!!$        first=.false.
!!$     endif
     if (i.lt.1.or.j.lt.1.or.k.lt.1.or.i.gt.ngrid.or.j.gt.ngrid.or.k.gt.ngrid) then
        part_out=part_out+1
        !write(*,*)'idx =',idx
     endif
     i=min(max(i,1),ngrid)
     j=min(max(j,1),ngrid)
     k=min(max(k,1),ngrid)
     lst(idx)=label(i,j,k)
     label(i,j,k)=idx
  enddo
  if (verbose.and.part_out.ne.0) write(*,*)'WARNING, part_out =',part_out

end subroutine llist_auto
