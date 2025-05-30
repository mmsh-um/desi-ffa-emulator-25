!> last modified 27-03-2017

subroutine cartco(x,y,z,ra,dec,d)
  
  implicit none
  
  real(8) :: x,y,z,ra,dec,d
  real(8), parameter :: pi=3.1415926535897932d0
  real(8), parameter :: deg2rad=pi/180.d0
  !real(8), parameter :: rad2deg=180.d0/pi
  real(8) :: ra_rad,dec_rad,d_rad

  ra_rad=ra*deg2rad
  dec_rad=dec*deg2rad

  x=d*sin(ra_rad)*cos(dec_rad)
  y=d*cos(ra_rad)*cos(dec_rad)
  z=d*sin(dec_rad)

!!$  x=d*sin(ra)*cos(dec)
!!$  y=d*cos(ra)*cos(dec)
!!$  z=d*sin(dec)

end subroutine cartco
