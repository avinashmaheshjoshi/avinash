PROGRAM TEST13
	IMPLICIT NONE
	REAL :: start,finish
	REAL :: tresults(15)
	INTEGER :: i,j,k,T13(400000),T13_1(400000),c(400000)
	DO k = 1,15
		CALL cpu_time(start)
		DO i = 1,400000		
			T13(i) = i + 7				
			T13_1(i) = i + 57
			CALL GCD(T13(i),T13_1(i),c(i))
		END DO
		CALL cpu_time(finish)		
		tresults(k) = 	finish - start
	END DO
	WRITE(*,*) sum(tresults)/15
END PROGRAM TEST13

!*****************************************************************************
!Functions and Subroutines used.
!*****************************************************************************
SUBROUTINE GCD(a,b,c)
	IMPLICIT NONE
	INTEGER   :: a, b, c

   IF (a < b) THEN       ! since a >= b must be true, they
      c = a              ! are swapped if a < b
      a = b
      b = c
   END IF

   DO                    ! now we have a <= b
      c = MOD(a, b)      !    compute c, the reminder
      IF (c == 0) EXIT   !    if c is zero, we are done.  GCD = b
      a = b              !    otherwise, b becomes a
      b = c              !    and c becomes b
   END DO                !    go back
END SUBROUTINE GCD
   

