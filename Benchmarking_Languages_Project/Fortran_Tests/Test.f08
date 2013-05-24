PROGRAM  Test
  IMPLICIT NONE
	INTEGER :: i,j
	REAL  :: a
	LOGICAL :: d
	a = 1e-400
	WRITE(*,*) a  	
END PROGRAM  Test

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
   

