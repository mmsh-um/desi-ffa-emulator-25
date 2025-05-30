! last modified 03-10-2016
 
subroutine itg2chr(itg,chr)
  
  implicit none
  
  integer(4), parameter :: ndigits=3
  integer(4) :: itg
  character(ndigits) :: chr
  integer(4) :: i,t,c
  logical :: zero

  !zero=.true.
  zero=.false.
  chr=''
  t=itg
  do i=ndigits-1,0,-1
     c=t/10**i
     if (c.gt.0.or.zero) then
        chr=trim(chr)//char(c+48)
        t=mod(t,10**i)
        zero=.true.
     endif
  enddo
  
end subroutine itg2chr
