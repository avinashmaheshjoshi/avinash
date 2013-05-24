PROGRAM Test16
	IMPLICIT NONE
	INTEGER :: i,j,k,nParams = 2
	REAL :: start,finish,tresults(15), lb(2), ub(2), res(3)
	lb(1) = -5
	lb(2) = -5
	ub(1) = 5
	ub(2) = 5

	do k = 1,15
		CALL cpu_time(start)
		call runSA(nParams,lb,ub,res)
		CALL cpu_time(finish)
		tresults(k) = finish - start
	end do
	WRITE(*,*) sum(tresults)/15
END PROGRAM Test16

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

subroutine runSA(nParams,lowerBounds,upperBounds,ret)
implicit none
	 INTEGER :: nParams
	 REAL :: lowerBounds(nParams),upperBounds(nParams) 
	 REAL :: cooling_sched,maxIters,eMin,iterationIndicator, xVec(nParams)
	 REAL :: eNew,eBest,e,k,ap,newX(nParams)
	 REAL :: ret(nParams + 1)
	 DOUBLE PRECISION :: stopTemp, T  
	 cooling_sched = 0.999
	 maxIters = 1e30
	 stopTemp = 1d-150
	 eMin = 0.000001
	 iterationIndicator = 1000
	 call neighFnc(nParams,lowerBounds,upperBounds,xVec)
	 call energyFnc(xVec,eBest)
	 e = eBest;
	 k = 1
	 T = 1

	 do while (k < maxIters .AND. e >= eMin .AND. T > stopTemp)
	  call neighFnc(nParams,lowerBounds,upperBounds,newX)
	  call energyFnc(newX,eNew)
	  if (eNew < eBest) then
	   eBest = eNew
	   e = eNew
	   xVec = newX
	  call RANDOM_NUMBER(ap)	 
	  else if(ap < exp((e-eNew)/(k*T))) then
	   eBest = eNew
	   e = eNew
	   xVec = newX
	  end if
	  k = k+1
	  T = cooling_sched*T
	 end do
	ret(1) = eBest
	ret(2) = xVec(1)
	ret(3) = xVec(2)
end subroutine runSA

subroutine energyFnc(x,ret)
	IMPLICIT NONE
	REAL :: x(2),ret,pi = 22/7
	ret = 10*2 + (x(1)**2 - 10*cos(2*pi*x(1))) + (x(2)**2 - 10*cos(2*pi*x(2)))
end subroutine energyFnc

subroutine neighFnc(nParams,lowerBounds,upperBounds,xVec)
	IMPLICIT NONE
	INTEGER :: nParams,i
	REAL :: xVec(nParams),lowerBounds(nParams),upperBounds(nParams),r(nParams)
		call init_random_seed()
		call RANDOM_NUMBER(r)
		xVec = (upperBounds - lowerBounds)*r - lowerBounds
end subroutine neighFnc
