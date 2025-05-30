!> last modified 07/05/20121  

subroutine write_time_and_date()

  implicit none

  character(8) :: date
  character(10) :: time
  
  call date_and_time(date,time)

  write(*,*)date(7:8),'/',date(5:6),'/',date(1:4),'   ',time(1:2),':',time(3:4),':',time(5:10)

end subroutine write_time_and_date
