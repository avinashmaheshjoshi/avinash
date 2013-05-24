a = randi((100,1000),400000)
b = randi((100,1000),400000)
T13_1 = zeros(400000)
tic()
for j in 1:15
for i in 1:400000
T13_1[i]=gcd(a[i],b[i])
end
end
timing = toc()
println(timing/15)
