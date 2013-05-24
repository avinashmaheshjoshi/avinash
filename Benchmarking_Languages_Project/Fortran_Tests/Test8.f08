PROGRAM TEST8
	IMPLICIT NONE
	REAL :: start,finish,random_normal,FindDet,T8(2500,2500),T8_1
	REAL :: tresults(15)
	INTEGER :: INFO,i,j,k
	DO k = 1,15
		CALL cpu_time(start)
		DO i = 1,2500			
			DO j=1,2500
				CALL init_random_seed()
				T8(i,j) = random_normal()
			END DO
		END DO
		T8_1 = FindDet(T8,3)
		CALL cpu_time(finish)		
		tresults(k) = 	finish - start
	END DO
	WRITE(*,*) sum(tresults)/15		
END PROGRAM TEST8


!*****************************************************************************
!Functions and Subroutines used.
!*****************************************************************************

SUBROUTINE init_random_seed()
	INTEGER :: i, n, clock
	INTEGER, DIMENSION(:), ALLOCATABLE :: seed
          
	CALL RANDOM_SEED(size = n)
	ALLOCATE(seed(n))
	CALL SYSTEM_CLOCK(COUNT=clock)
	seed = clock + 37 * (/ (i - 1, i = 1, n) /)
	CALL RANDOM_SEED(PUT = seed)
	DEALLOCATE(seed)
END SUBROUTINE


FUNCTION random_normal() RESULT(fn_val)

! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

REAL :: fn_val

!     Local variables
REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,           &
            r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

DO
  CALL RANDOM_NUMBER(u)
  CALL RANDOM_NUMBER(v)
  v = 1.7156 * (v - 0.5)

!     Evaluate the quadratic form
  x = u - s
  y = ABS(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
  IF (v**2 < -4.0*LOG(u)*u**2) EXIT
END DO

!     Return ratio of P's coordinates as the normal deviate
fn_val = v/u
RETURN

END FUNCTION random_normal

REAL FUNCTION FindDet(matrix, n)
	IMPLICIT NONE
	REAL, DIMENSION(n,n) :: matrix
	INTEGER, INTENT(IN) :: n
	REAL :: m, temp
	INTEGER :: i, j, k, l
	LOGICAL :: DetExists = .TRUE.
	l = 1
	!Convert to upper triangular form
	DO k = 1, n-1
		IF (matrix(k,k) == 0) THEN
			DetExists = .FALSE.
			DO i = k+1, n
				IF (matrix(i,k) /= 0) THEN
					DO j = 1, n
						temp = matrix(i,j)
						matrix(i,j)= matrix(k,j)
						matrix(k,j) = temp
					END DO
					DetExists = .TRUE.
					l=-l
					EXIT
				ENDIF
			END DO
			IF (DetExists .EQV. .FALSE.) THEN
				FindDet = 0
				return
			END IF
		ENDIF
		DO j = k+1, n
			m = matrix(j,k)/matrix(k,k)
			DO i = k+1, n
				matrix(j,i) = matrix(j,i) - m*matrix(k,i)
			END DO
		END DO
	END DO
	
	!Calculate determinant by finding product of diagonal elements
	FindDet = l
	DO i = 1, n
		FindDet = FindDet * matrix(i,i)
	END DO
	
END FUNCTION FindDet

