!program:
!this program is based on elimination of unkowns by Gauss.
!it's goal is to get some matrix's inverse matrix
!history:
!2011/-/-	don	first release
!2012/3/12	don	second release(changes:rectify some logical fault)
program main
use      sub	
write(*,*)"input degree"
	read(*,*)n
	call subm(n)
!	 write(*,*)((B(i,j),j=1,N),i=1,N)
print *,"finished"
end program
