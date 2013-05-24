var timing = DenseVector.zeros[Long](15)
for(i <- 0 until 15){
	
	val start = System.currentTimeMillis()
	val T11 = DenseVector.randi(1000,3500000)
	val phi = 1.618033988749895
	val T11_1 = (phi:^T11 - (-phi):^(-T11))/sqrt(5.0)	
	val finish = System.currentTimeMillis()
	timing(i) = (finish - start)
}

println(mean(timing)/1000)


