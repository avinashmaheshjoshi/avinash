tic()
for i in 1:15
T14 = zeros(Int64,500,500)
 for j in 1:500
  for k in 1:500
   T14[k,j] = abs(j-k) + 1
   end
  end
end
timing = toc()
println (timing/15)
