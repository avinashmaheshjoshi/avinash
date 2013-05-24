load("corrFunc.jl")
p = 0; vt = 0; vr = 0; vrt = 0; rvt = 0; RV = 0; j = 0; k = 0; vr1 = 0
x2 = 0; R = 0; Rxx = 0; Ryy = 0; Rxy = 0; Ryx = 0; Rvmax = 0
function Trace(y) sum(diag(y));end
for i in 1:15
 tic()  
 x = reshape(rand(45*45),45,45)
    # Calculation of Escoufier's equivalent vectors
    p = size(x)[2]
    vt = [1:p]                                  # Variables to test
    vr = zeros(Int32,p)                                 # Result: ordered variables
    RV = zeros(Int32,p)                                  # Result: correlations
    vrt = 0
    vr1 = [0]
    for j in 1:p  
    {                           # loop on the variable number
     Rvmax = 0
     for k in [1:(p-j+1)]                       # loop on the variables
     {                 	
	if (vr1[1] == 0)
	{
	 x2 = hcat(x, x[:,vt[k]])
	}
	elseif (vr1!=0)
	{ 
         x2 = hcat(x, x[:,vr1], x[:,vt[k]])
	}
	end
        R = corrmat(x2)                          # Correlations table
        Ryy = R[1:p, 1:p]
        Rxx = R[(p+1):(p+j), (p+1):(p+j)]
        Rxy = R[(p+1):(p+j), 1:p]
        Ryx = transpose(Rxy)
        rvt = Trace(Ryx * Rxy) / sqrt(Trace(Ryy * Ryy) * Trace(Rxx * Rxx)) # RV calculation
        if (rvt > Rvmax) 
         {
          Rvmax = rvt                         # test of RV
          vrt = vt[k]                         # temporary held variable
         }
         end
     }
     end
     vr[j] = vrt
     vr1 = vr[vr != 0]                         # Result: variable
     RV[j] = Rvmax                        # Result: correlation
     vt = vt[vt!=vr[j]]                      # reidentify variables to test
    }
    end
  toc()
end
