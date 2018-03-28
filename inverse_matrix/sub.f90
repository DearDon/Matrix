module sub
	contains
	
		subroutine subm(N)
		integer::N
		real*8 a(N,N),B(N,N),x
		
		write(*,*)"input the Matrix"
		read(*,*)((A(i,j),j=1,N),i=1,N)
		do i=1,N
			do j=1,N
				if(i==j) then
				B(i,j)=1
				else
				B(i,j)=0
				end if
			end do
		end do
		do j=1,N
			x=A(j,j)
			do k=1,N
				A(j,k)=A(j,k)/x
				B(j,k)=B(j,k)/x
				
			end do
			do i=1,N
				if(i/=j)then
				x=A(i,j)
				do k=1,N
				A(i,k)=A(i,k)-A(j,k)*x
				B(i,k)=B(i,k)-x*B(j,k)
				end do
				end if
			end do
		end do
		write(*,'(f22.6)')((B(i,j),j=1,N),i=1,N)
		
		end subroutine subm

end module
