tic()
for j in 1:15
T11 = randi((100,1000),3500000)
phi = 1.618033988749895
fib = Array(Float64,3500000)
for i in 1:3500000
fib[i] = (phi^T11[i] - (-phi)^(-T11[i]))/sqrt(5)
end
end
timing = toc()
println (timing/15)
