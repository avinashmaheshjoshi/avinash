tic()
for i in (1:15)
T9 = reshape(randn(3000*3000),3000,3000)
T9 = transpose(T9) * T9
T9_1 = chol(T9)
end
timing = toc()
println(timing/15)
