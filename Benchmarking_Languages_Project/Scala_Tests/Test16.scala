import breeze.numerics._
import breeze.linalg._
import breeze.stats._
//
//Enerngy Function
//
def energyFnc(x: DenseVector[Double]):Double = {
	val ret = 10*2 + (x(0)*x(0) - 10*cos(2*22/7*x(0))) + (x(1)*x(1) - 10*cos(2*22/7*x(1)))
	return ret
}

//
//Negihbourhood Function
//
def neighFnc(nParams: Int,lowerBounds: DenseVector[Double],upperBounds: DenseVector[Double]): DenseVector[Double] = {
 var Nhood = DenseVector.zeros[Double](nParams)
 for(i <- 0 until nParams){
  Nhood(i) = distributions.Uniform(lowerBounds(i),upperBounds(i)).draw() 
 }
 return Nhood
}

//
//Simulated Annealing Algorithm
//
def runSA(nParams: Int,lowerBounds: DenseVector[Double],upperBounds: DenseVector[Double]): DenseVector[Double] = {  
val cooling_sched = 0.999
val maxIters = 1e15
val stopTemp = 1e-150
val eMin = 1e-150
val iterationIndicator = 1000
var xVec = neighFnc(nParams,lowerBounds,upperBounds)
var eBest = energyFnc(xVec)
var e = eBest
var k=1
var T=1.0
var result = DenseVector.zeros[Double](nParams + 1)
 while (k < maxIters & e >= eMin & T > stopTemp){
  var newX = neighFnc(nParams,lowerBounds,upperBounds)
  var eNew = energyFnc(newX)
  if (eNew < eBest){ 
   eBest = eNew
   e = eNew
   xVec = newX
  }	 
  else if (distributions.Uniform(0,1).draw() < exp((e-eNew)/(k*T))){
   eBest = eNew
   e = eNew
   xVec = newX
  }
  k = k + 1  
  T = cooling_sched * T
 }
 result(0) = eBest
 result(1) = xVec(0)
 result(2) = xVec(1)
 return result	
}

//Calling SA

val lb = DenseVector(-5.1,-5.1)
val ub = DenseVector(5.1,5.1)
var timing = DenseVector.zeros[Long](15)

for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	runSA(2,lb,ub)	
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
	println(timing(i))
}

println(timing.sum)



