!> last modified 05-04-2017

subroutine unisphe(n,x,y,z,u)

  implicit none
      
  integer(4) :: n
  real(8), dimension(n) :: x,y,z
  real(8), dimension(4,n) :: u
  integer(4) :: i
  real(8) :: ra,dec,d

  do i=1,n
!!$     call polaco(x(i),y(i),z(i),ra,dec,d)
!!$     u(1,i)=sin(ra)*cos(dec)
!!$     u(2,i)=cos(ra)*cos(dec)
!!$     u(3,i)=sin(dec)
!!$     u(4,i)=d
     d=sqrt(x(i)**2+y(i)**2+z(i)**2)
     u(1,i)=x(i)/d
     u(2,i)=y(i)/d
     u(3,i)=z(i)/d
     u(4,i)=d
  enddo
  
end subroutine unisphe
