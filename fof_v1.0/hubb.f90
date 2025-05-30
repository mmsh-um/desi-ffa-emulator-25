!> last modified 05-04-2019

function hubb(z)
  
  implicit none
  
  include 'PRM_cosmo.f90'
  
  real(8) :: hubb,z
  logical, save :: first=.true.
  
  if (use_hubb_in) then
     if (first) then
        write(*,*)'using given Hubble function:'
        write(*,*)' z       =',z,'hubb_in =',hubb_in
        first=.false.
     endif
     hubb=hubb_in
  else
     if (first) then
        write(*,*)'hubb0  =',hubb0
        write(*,*)'omg_m0 =',omg_m0
        write(*,*)'omg_l0 =',omg_l0
        write(*,*)'omg_k0 =',omg_k0
        first=.false.
     endif
     hubb=hubb0*sqrt(omg_m0*(1+z)**3+omg_k0*(1+z)**2+omg_l0)
  endif
  
  return
end function hubb
