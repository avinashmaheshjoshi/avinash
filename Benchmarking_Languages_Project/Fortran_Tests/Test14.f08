PROGRAM TEST14
	IMPLICIT NONE
	REAL :: start,finish,random_normal,T14(500,500)
	REAL :: tresults(15)
	INTEGER :: i,j,k
	DO k = 1,15
		CALL cpu_time(start)
		DO i = 1,500		
			DO j=1,500
				T14(j,i) = abs(i - j) + 1
			END DO
		END DO
		CALL cpu_time(finish)		
		tresults(k) = 	finish - start
	END DO
	WRITE(*,*) sum(tresults)/15		
END PROGRAM TEST14


