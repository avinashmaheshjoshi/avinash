tic()
for i in (1:15)
T10 = reshape(randn(1600*1600),1600,1600)
T10_1 = inv(T10)
end
timing = toc()
println(timing/15)
