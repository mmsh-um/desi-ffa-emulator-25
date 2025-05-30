!> last modified 01-02-2023

function sigmoid(x)
  
  implicit none

  real(8) :: sigmoid,x
  real(8), parameter :: pi=3.1415926535897932d0
  
  sigmoid=1/(1+exp(-x))
  !sigmoid=0.5*(atan(pi*x/2)*2/pi+1)                
  !sigmoid=erf(sqrt(pi)*x/2)
  !sigmoid=tanh(x)+1              
  !sigmoid=0.5*(x/(1+abs(x))+1)
 
end function sigmoid
