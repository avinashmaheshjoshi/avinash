load("SA.jl")

function energyFnc(x) 
 ret = 10*2 + (x[1]^2 - 10*cos(2*pi*x[1])) + (x[2]^2 - 10*cos(2*pi*x[2]))
 return ret
end

function neighFnc(nParams,lowerBounds,upperBounds)
 Nhood = zeros(nParams)
  for i in 1:2
   Nhood[i] = randi((lowerBounds[i],upperBounds[i])) + rand()
  end
 return Nhood
end

tic()
for i in 1:15
T16_1 = runSA(2,[-5,-5],[5,5],energyFnc,neighFnc)
end
timing = toc()
println(timing/15)
