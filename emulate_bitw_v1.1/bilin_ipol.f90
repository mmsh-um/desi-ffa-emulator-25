!> last modified 01-02-2023

function bilin_ipol(x_in,y_in,f,ngrid_x,step_x,zerogrid_x,ngrid_y,step_y,zerogrid_y)
  
  implicit none

  integer(4) :: ngrid_x,ngrid_y
  real(8) :: bilin_ipol,x_in,y_in
  real(8) :: step_x,step_y,zerogrid_x,zerogrid_y
  real(8), dimension(ngrid_x,ngrid_y) :: f
  real(8) :: x,y
  integer(4) :: i0,j0,i1,j1
  real(8) :: step
  real(8) :: xd,yd
  real(8) :: x0,y0
  real(8) :: c00,c01,c10,c11
  real(8) :: c0,c1
  real(8) :: c
  logical, save :: first=.true.

  bilin_ipol=0.d0

  x=x_in-zerogrid_x
  y=y_in-zerogrid_y
  
  if (x.lt.0.d0.or.x.gt.step_x*ngrid_x.or.y.lt.0.d0.or.y.gt.step_y*ngrid_y) then
     if (first) then
        write(*,*)'WARNING, there are particles out of bounds!!!'
        first=.false.
     endif
     x=min(max(x,0.d0),step_x*ngrid_x)
     x=min(max(y,0.d0),step_y*ngrid_y)
  else
     
     i0=int(x/step_x+0.5d0)
     j0=int(y/step_y+0.5d0)
     i1=int(x/step_x+1.5d0)
     j1=int(y/step_y+1.5d0)

     !x0=(i0-0.5d0)*step_x
     !y0=(j0-0.5d0)*step_y

     x0=(i0-1)*step_x
     y0=(j0-1)*step_y
         
     xd=(x-x0)/step_x
     yd=(y-y0)/step_y
     
!> periodic conditions
     !i0=i0+(ngrid_x-i0)/ngrid_x*ngrid_x
     !j0=j0+(ngrid_y-j0)/ngrid_y*ngrid_y
     !i1=i1-(i1/(ngrid_x+1)*ngrid_x)
     !j1=j1-(j1/(ngrid_y+1)*ngrid_y) 

     c00=f(i0,j0)
     c01=f(i0,j1)
     c10=f(i1,j0)
     c11=f(i1,j1)
     
     c0=c00*(1-xd)+c10*xd
     c1=c01*(1-xd)+c11*xd
     
     c=c0*(1-yd)+c1*yd
     
     bilin_ipol=c
 
  endif
  
end function bilin_ipol
