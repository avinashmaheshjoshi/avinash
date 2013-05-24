tic()
for i in 1:15
T4 = reshape(randn(2800*2800),2800,2800)
T4_1 = transpose(T4) * T4
end
timing = toc()
println (timing/15)
