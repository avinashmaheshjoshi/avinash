program test_random_number
	REAL :: r(3,3),start,finish
	REAL gennor
	EXTERNAL gennor, snorm
	EXTERNAL getsd
	Do i = 1,3
		DO j = 1,3
			r(i,j)=i+j
		end do
	WRITE(*,*) r(i,:)
	end do 
CALL DGEMM(r,r)
END program