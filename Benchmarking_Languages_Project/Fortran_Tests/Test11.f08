PROGRAM TEST11
	IMPLICIT NONE
	REAL :: start,finish,random_normal,tresults(15),phi = 1.618033988749895
	REAL*16 :: T11(3500000) 
	INTEGER :: i,j,k,INFO
	DO k = 1,15
		CALL cpu_time(start)
		DO i = 1,3500000
			CALL init_random_seed()
			T11(i) = (phi**(floor(random_normal()*1000)) - (-phi)**(-floor(random_normal()*1000)))/sqrt(5.0)
			!T11(i) = floor(random_normal()*1000)
		END DO
		!T11_1 = ((phi**T11) - (-phi)**(-T11))/sqrt(5.0)
		CALL cpu_time(finish)		
		tresults(k) = 	finish - start
	END DO
	WRITE(*,*) sum(tresults)/15		
END PROGRAM TEST11

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
