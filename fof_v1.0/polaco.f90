!> last modified 27-03-2017

subroutine polaco(x,y,z,ra,dec,d)
  
  implicit none
  
  real(8) :: x,y,z,ra,dec,d
  real(8), parameter :: pi=3.1415926535897932d0
  !real(8), parameter :: deg2rad=pi/180.d0
  real(8), parameter :: rad2deg=180.d0/pi

  d=sqrt(x**2+y**2+z**2)
  ra=atan2(x,y)
  if (ra.lt.0.d0) ra=2*pi+ra
  dec=asin(z/d)
  
  ra=ra*rad2deg
  dec=dec*rad2deg

end subroutine polaco
