//var timing = DenseVector.zeros[Long](15)
//for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	var T4 = DenseMatrix.randn(2800,2800)
	var T4_1 = T4.t * T4
	val finish = System.currentTimeMillis()
	val timing = (finish - start)
//}
println(timing/1000)


