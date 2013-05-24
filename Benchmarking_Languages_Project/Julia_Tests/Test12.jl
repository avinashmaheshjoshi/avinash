tic()
for i in 1:15
T12 = repmat(reshape([1:3000],3000,1),1,3000)
ad = repmat(reshape([0:2999],3000,1),1,3000)
T12 = 1/(transpose(T12) + ad)
end
timing = toc()
println (timing/15)
