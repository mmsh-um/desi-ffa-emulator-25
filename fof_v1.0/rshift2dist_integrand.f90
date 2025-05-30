!> last modified 05-04-2019

function rshift2dist_integrand(z)
  
  implicit none
  
  real(8) :: rshift2dist_integrand,z
  real(8), parameter :: c=299792.458d0
  real(8), external :: hubb
  
  rshift2dist_integrand=c/hubb(z)
  
  return
end function rshift2dist_integrand
