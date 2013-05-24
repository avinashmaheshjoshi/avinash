PROGRAM Test15
	use random
	IMPLICIT NONE
	INTEGER :: i,j,k,N=50000,thin=1000
	REAL :: T15(50000,2),start,finish,tresults(15),func15(50000,2)
	REAL :: x, y
	LOGICAL :: first 
	do k = 1,15
		CALL cpu_time(start)
		DO i = 1,N
        	DO j = 1,thin
            	x = random_gamma(3.0,first) * (y*y + 4)
            	y = 1/(x + 1) + random_normal()/sqrt(2*(x + 1))
        	END DO
        T15(i,:) = (/x,y/)
    	END DO
		CALL cpu_time(finish)
		tresults(k) = finish - start
	end do
	WRITE(*,*) sum(tresults)/15
END PROGRAM Test15




