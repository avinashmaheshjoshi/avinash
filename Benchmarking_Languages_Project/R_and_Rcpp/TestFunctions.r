##############################################
## Coded functions
##############################################

## De Jong's function 1
## dJngF1
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b01

## Axis parallel hyper-ellipsoid function
## axPrlHypElpsF
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b01

## Rotated hyper-ellipsoid function
## rotHypElpsF
## -65.536 <= x[i] <= 65.536
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b02

## Moved axis parallel hyper-ellipsoid function
## mvdAxPrlHypElpsF
## -5.12i <= x[i] <= 5.12i
## min: f(x[i] = 5i, i = 1:n) = 0
## use build_b03

## Rosenbrock's valley (De Jong's function 2) (banana function)
## rsnbrkVlyF
## -2.048 <= x[i] <= 2.048
## min: f(x[i] = 1, i = 1:n) = 0
## use build_b04

## Rastrigin's function
## rstrgnF
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b01

## Schwefel's function
## schwflF
## -500 <= x[i] <= 500
## min: f(x[i] = 420.9687, i = 1:n) = -418.9829n
## use build_b05

## Griewangk's function
## grwngkF
## -600 <= x[i] <= 600
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b06

## Sum of different power function
## smOfDffPwrF
## -1 <= x[i] <= 1
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b07

## Ackley's Path function
## acklyPthF
## -32.868 <= x[i] <= 32.868
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b08

## Michalewicz's function
## mchlwczF
## 0 <= x[i] <= pi
## min: f(x[i] = ?, i = 1:n, n = 5) = -4.687
## min: f(x[i] = ?, i = 1:n, n = 10) = -9.66
## use build_b09

## Langermann's function
## lngrmnF
## 0 <= x[i] <= 10
## min: f(?,?) = -1.4
## use build_b10

## Branins's function
## brnF
## -5 <= x[1] <= 10
## 0 <= x[2] <= 15
## min: f((-pi,12.275) = 0.397887
## min: f((pi,2.275) = 0.397887
## min: f((9.42478,2.475) = 0.397887
## use build_b11

## Easom's function
## esmF
## -100 <= x[i] <= 100
## min: f((pi,pi)) = -1
## use build_b12

## Goldstein-Price's function
## gldPrcF
## -2 <= x[i] <= 2
## min: f((0,-1)) = 3
## use build_b13

## Six-hump camel back function
## sxHmpCmlBckF
## -3 <= x[1] <= 3
## -2 <= x[2] <= 2
## min: f((-0.0898,0.7126)) = -1.0316
## min: f((0.0898,-0.7126)) = -1.0316
## use build_b14

## De Jong's function 5
## dJngF5
## -65.536 <= x[i] <= 65.536
## min: f(?,?) = ?
## use build_b02

## "Drop-wave" function
## drpWvF
## -5.12 <= x[i] <= 5.12
## min: f((?,?)) = ?
## use build_b01

## Schubert's function
## schbrtF
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = ?, i = 1:n, n = 2) = ?
## use build_b01

## Goutham's test function
## gTstFn
## 0 <= x[1] <= 12
## 0 <= x[2] <= 9
## 0 <= x[3] <= 9
## 0 <= x[4] <= 8
## 0 <= x[5] <= 10
## 0 <= x[6] <= 8
## 0 <= x[7] <= 6
## sum(x) <= 20
## min: f(x[i] = ?, i = 1:n, n = 7) = ?
## use build_bgtf

## wgTstFn1
## 4.08672   <= x[1] <= 110.34144
## 11.147933 <= x[2] <= 200.6628
## 8.0891    <= x[3] <= 242.673
## 6.36498   <= x[4] <= 126.3249
## min: f((4.16016,76.5438,8.1673,6.36498)) = 231769.0329
## use build_bwgtf

## wgTstFn2
## 3.1659525 <= x[1] <= 63.31905
## 6.49038   <= x[2] <= 207.69216
## 4.90759   <= x[3] <= 103.05939
## 5.916     <= x[4] <= 165.648
## min: f((59.56381,6.56152,4.92277,5.964)) = 303049.5018
## use build_bwgtf

## wgTstFn3
## 4.98774   <= x[1] <= 134.66898
## 9.1396    <= x[2] <= 182.792
## 3.2720825 <= x[3] <= 65.44165
## 1.9096    <= x[4] <= 45.8304
## min: f((39.56322,9.1792,3.28833,1.9384)) = 32811.65832
## use build_bwgtf

## wgTstFn4
## 10.17159  <= x[1] <= 203.4318
## 2.52831   <= x[2] <= 91.01916
## 9.14118   <= x[3] <= 246.81186
## 4.38668   <= x[4] <= 122.82704
## min: f((10.24318,51.51324,9.22354,4.44672)) = 77385.74832
## use build_bwgtf

## wgTstFn5
## 4.05525   <= x[1] <= 60.82875
## 6.8036267 <= x[2] <= 183.69792
## 3.19941   <= x[3] <= 38.39292
## 8.77455   <= x[4] <= 157.9419
## min: f((37.56575,6.81088,3.29882,8.8491)) = 35441.58242
## use build_bwgtf

## wgDelFn



##############################################
## Uncoded functions (not finding a complete
## set of constants/functional forms)
##############################################

## Shekel's foxholes function
## Deceptive functions



##############################################
## Function definitions
##############################################

## De Jong's function 1 (sphere model)
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b01

dJngF1 <- function(x){
	if((length(x[x > 5.12]) > 0) | (length(x[x < -5.12]) > 0)){
		y = 1e9
	}
	else{
		y = sum(x^2)
	}
	return(y)
}

plotdJngF1 <- function(x1,x2){
	y = (x1^2+x2^2)
	return(y)
}


## Axis parallel hyper-ellipsoid function (weighted sphere model)
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b01

axPrlHypElpsF <- function(x){
	if((length(x[x > 5.12]) > 0) | (length(x[x < -5.12]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = (i*(x[i]^2))
		}
		y = sum(yy)
	}
	return(y)
}


plotaxPrlHypElpsF <- function(x1,x2){
		y = (x1^2+x2^2)
		return(y)
}


## Rotated hyper-ellipsoid function (Schwefel's function 1.2)
## -65.536 <= x[i] <= 65.536
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b02

rotHypElpsF <- function(x){
	if((length(x[x > 65.536]) > 0) | (length(x[x < -65.536]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = (sum(x[1:i]))^2
		}
		y = sum(yy)
	}
	return(y)
}

plotrotHypElpsF <- function(x1,x2){
		y = (x1^2+x2^2)
		return(y)
	
}

## Moved axis parallel hyper-ellipsoid function
## -5.12i <= x[i] <= 5.12i
## min: f(x[i] = 5i, i = 1:n) = 0
## use build_b03

mvdAxPrlHypElpsF <- function(x){
	cstrMet = TRUE
	n = length(x)
	for(i in 1:n)
	{
		if((x[i] > 5.12*i) | (x[i] < -5.12*i)){
			y = 1e9
			break
			cstrMet = FALSE
		}
	}
	if(cstrMet){
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = (i*(x[i] - 5*i))^2
		}
		y = sum(yy)
	}
	return(y)
}

plotmvdAxPrlHypElpsF <- function(x1,x2){
	z <- complex(imaginary=5)
	y <- z*(x1^2+x2^2)
	return(y)
}

## Rosenbrocks' valley (De Jong's function 2)
## -2.048 <= x[i] <= 2.048
## min: f(x[i] = 1, i = 1:n) = 0
## use build_b04

rsnbrkVlyF <- function(x){
	if((length(x[x > 2.048]) > 0) | (length(x[x < -2.048]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,(length(x) - 1))
		for(i in 1:(length(x) - 1)){
			yy[i] = 100*((x[i+1] - (x[i])^2)^2) + (1 - x[i])^2
		}
		y = sum(yy)
	}
	return(y)
}

plotrsnbrkVlyF <- function(x,y){
	z <- 100*(y-x)^2 +(1-x)^2
	return(z)
}

## Rastrigin's function
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b01

rstrgnF <- function(x){
	if((length(x[x > 5.12]) > 0) | (length(x[x < -5.12]) > 0)){
		y = 1e9
	}
	else{
		n = length(x)
		y = sum((x)^2,-10*cos(2*pi*x),10*n)
	}
	return(y)
}

plotrstrgnF <- function(x,y){
	z <- 100 + (x^2 - 10*cos(2*pi*x)) + (y^2 - 10*cos(2*pi*y))
	return(z)
}

## Schwefel's function
## -500 <= x[i] <= 500
## min: f(x[i] = 420.9687, i = 1:n) = -418.9829n
## use build_b05

schwflF <- function(x){
	if((length(x[x > 500]) > 0) | (length(x[x < -500]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = prod(-1,x[i],sin(sqrt(sqrt(x[i]^2))))
		}
		y = sum(yy)
	}
	return(y)
}

plotschwflF <- function(x,y){
	z <- -x*sin(sqrt(abs(x))) - y*sin(sqrt(abs(y)))	
	return(z)
}

## Griewangk's function
## -600 <= x[i] <= 600
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b06

grwngkF <- function(x){
	if((length(x[x > 600]) > 0) | (length(x[x < -600]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = cos((x[i])/(sqrt(i)))
		}
		y = sum(((x^2)/(4000)),(-1*prod(yy)),1)
	}
	return(y)
}

plotgrwngkF <- function(x,y){
	z <- (x^2+y^2)/4000 - (cos(x/sqrt(1))*cos(x/sqrt(2)))
	return(z)
}

## Sum of different power function
## -1 <= x[i] <= 1
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b07

smOfDffPwrF <- function(x){
	if((length(x[x > 1]) > 0) | (length(x[x < -1]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = (sqrt(x[i]^2))^(i + 1)
		}
		y = sum(yy)
	}
	return(y)
}

plotsmOfDffPwrF <- function(x,y){
	z <- abs(x)^2 + abs(y)^3
	return(z)
}

## Ackley's Path function
## -32.868 <= x[i] <= 32.868
## min: f(x[i] = 0, i = 1:n) = 0
## use build_b08

acklyPthF <- function(x){
	if((length(x[x > 32.868]) > 0) | (length(x[x < -32.868]) > 0)){
		y = 1e9
	}
	else{
		n = length(x)
		y = sum((-20*exp(-0.2*sqrt(sum(x^2)/n))),(-1*exp(sum(cos(2*pi*x))/n)),20,exp(1))
	}
	return(y)
}

plotacklyPthF <- function(x,y){
	xx <- c(x,y)
	z <- acklyPthF(xx)
	return(z)
}

## Michalewicz's function
## 0 <= x[i] <= pi
## min: f(x[i] = ?, i = 1:n, n = 5) = -4.687
## min: f(x[i] = ?, i = 1:n, n = 10) = -9.66
## use build_b09

mchlwczF <- function(x){
	if((length(x[x > pi]) > 0) | (length(x[x < 0]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		for(i in 1:length(x)){
			yy[i] = (sin(x[i]))*((sin((i*(x[i]^2))/pi))^20)
		}
		y = -1*sum(yy)
	}
	return(y)
}

plotmchlwczF <-  function(x,y){
	xx <- c(x,y)
	for(i in 1:length(xx)){
			yy[i] = (sin(xx[i]))*((sin((i*(xx[i]^2))/pi))^20)
		}
		z = -1*sum(yy)

	return(z)
}


## Langermann's function
## 0 <= x[i] <= 10
## min: f(x[i] = ?, i = 1:n, n = 2) = -1.4
## use build_b10

lngrmnF <- function(x){
	if((length(x[x > 10]) > 0) | (length(x[x < 0]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,length(x))
		a = c(3,5,2,1,7)
		b = c(5,2,1,4,9)
		c = c(1,2,5,2,3)
		for(i in 1:length(a)){
			yy[i] = c[i]*(exp((-1*((x[1] - a[i])^2 + (x[2] - b[i])^2))/pi))*(cos(pi*((x[1] - a[i])^2 + (x[2] - b[i])^2)))
		}
		y = sum(yy)
	}
	return(y)
}

plotlngrmnF <-  function(x1,x2){
	x <- c(x1,x2)
	z <- lngrmnF(x)
	return(z)
}

## Branins's function
## -5 <= x[1] <= 10
## 0 <= x[2] <= 15
## min: f((-pi,12.275) = 0.397887
## min: f((pi,2.275) = 0.397887
## min: f((9.42478,2.475) = 0.397887
## use build_b11

brnF <- function(x){
	if((x[1] < -5) | (x[2] < 0) | (x[1] > 10) | (x[2] > 15)){
		y = 1e9
	}
	else{
		y = sum(((sum(x[2],-1*(5.1/(4*(pi^2)))*(x[1]^2),(5/pi)*x[1],-6))^2),(10*(1 - (1/(8*pi)))*(cos(x[1]))),10)
	}
	return(y)
}

plotbrnF  <-  function(x1,x2){
	x <- c(x1,x2)
	z <- brnF(x)
	return(z)
}

## Easom's function
## -100 <= x[i] <= 100
## min: f((pi,pi)) = -1
## use build_b12

esmF <- function(x){
	if((length(x[x > 100]) > 0) | (length(x[x < -100]) > 0)){
		y = 1e9
	}
	else{
		y = -1*(cos(x[1]))*(cos(x[2]))*(exp(-1*((x - pi)%*%(x - pi))))
	}
	return(y)
}

plotesmF  <-  function(x1,x2){
	x <- c(x1,x2)
	z <- esmF(x)
	return(z)
}

## Goldstein-Price's function
## -2 <= x[i] <= 2
## min: f((0,-1)) = 3
## use build_b13

gldPrcF <- function(x){
	if((length(x[x > 2]) > 0) | (length(x[x < -2]) > 0)){
		y = 1e9
	}
	else{
		y = (sum(1,((sum(1,x))^2)*(sum(19,-14*(sum(x)),3*(sum(x^2)),6*prod(x)))))*(sum(30,((sum(2*x[1], -3*x[2]))^2)*(sum(18,-32*x[1],12*(x[1]^2),48*x[2],27*(x[2]^2),-36*x[1]*x[2]))))
	}
	return(y)
}

plotgldPrcF  <-  function(x1,x2){
	x <- c(x1,x2)
	z <- gldPrcF(x)
	return(z)
}

## Six-hump camel back function
## -3 <= x[1] <= 3
## -2 <= x[2] <= 2
## min: f((-0.0898,0.7126)) = -1.0316
## min: f((0.0898,-0.7126)) = -1.0316
## use build_b14

sxHmpCmlBckF <- function(x){
if((x[1] < -3) | (x[2] < -2) | (x[1] > 3) | (x[2] > 2)){
		y = 1e9
	}
	else{
		y = sum((sum(4,-2.1*(x[1]^2),((x[1]^4)/3)))*(x[1]^2),x[1]*x[2],(sum(-4,4*(x[2]^2)))*(x[2]^2))
	}
	return(y)
}

plotsxHmpCmlBckF <- function(x1,x2){
	x <- c(x1,x2)
	z <- sxHmpCmlBckF(x)
	return(z)
}

## De Jong's function 5
## -65.536 <= x[i] <= 65.536
## min: f(x[i] = ?, i = 1:n, n = 2) = ?
## use build_b02

dJngF5 <- function(x){
	if((length(x[x > 65.536]) > 0) | (length(x[x < -65.536]) > 0)){
		y = 1e9
	}
	else{
		yy = rep(NA,25)
		a = rbind(t(rep(c(-32,-16,0,16,32),5)),t(c(rep(-32,5),rep(-16,5),rep(0,5),rep(16,5),rep(32,5))))
		for(i in 1:25){
			yy[i] = 1/sum(i,(x[1] - a[1,i])^6,(x[2] - a[2,i]^6))
		}
		y = 1/sum(0.002,sum(yy))
	}
	return(y)
}

plotdJngF5 <-  function(x1,x2){
	x <- c(x1,x2)
	z <- dJngF5(x)
	return(z)
}

## "Drop-wave" function
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = ?, i = 1:n, n = 2) = ?
## use build_b01

drpWvF <- function(x){
	if((length(x[x > 5.12]) > 0) | (length(x[x < -5.12]) > 0)){
		y = 1e9
	}
	else{
		y = (-1)*((sum(1,cos(12*sqrt(x%*%x))))/(sum(0.5*(x%*%x),2)))
	}
	return(y)
}

plotdrpWvF <-  function(x1,x2){
	x <- c(x1,x2)
	z <- drpWvF(x)
	return(z)
}

## Schubert's function
## -5.12 <= x[i] <= 5.12
## min: f(x[i] = ?, i = 1:n, n = 2) = ?
## use build_b01

schbrtF <- function(x){
	if((length(x[x > 5.12]) > 0) | (length(x[x < -5.12]) > 0)){
		y = 1e9
	}
	else{
		yy1 = rep(NA,5)
		yy2 = rep(NA,5)
		for(i in 1:5){
			yy1[i] = i*cos(sum((i + 1)*x[1],i))#1 replaced by i in sum
			yy2[i] = i*cos(sum((i + 1)*x[2],i))#1 replaced by i in sum
		}
		y = prod(-1,sum(yy1),sum(yy2))
	}
	return(y)
}

plotschbrtF <-  function(x1,x2){
	x <- c(x1,x2)
	z <- schbrtF(x)
	return(z)
}

## Goutham's test function
## gTstFn
## 0 <= x[1] <= 12
## 0 <= x[2] <= 9
## 0 <= x[3] <= 9
## 0 <= x[4] <= 8
## 0 <= x[5] <= 10
## 0 <= x[6] <= 8
## 0 <= x[7] <= 6
## sum(x) <= 20
## min: f(x[i] = ?, i = 1:n, n = 7) = ?
## use build_bgtf
#Modified for maximization made exp -exp and ROI -1e50
gTstFn <- function(x){
	if(length(x) <= 2){
		x <- c(x,rep(0,5))
	}
	#print(length(x))
	if ((x[1] <= 12) && (x[1] >= 0) && (x[2] <= 9) && (x[2] >= 0) 
	&& (x[3] <= 9) && (x[3] >= 0) && (x[4] <= 8)&& (x[4] >= 0) 
	&& (x[5] <= 10) && (x[5] >= 0) && (x[6] <= 8) && (x[6] >= 0) 
	&& (x[7] <= 6) && (x[7] >= 0) && (sum(x) <= 20)){
		x1 <- c(5 + x[1]^2,x[2]^3,23 + x[3],5 - 3*(x[5]^2),16 + x[1]^2,
			x[3] + x[4] + x[6]*x[7],x[2] - x[4])
		ROI <- -exp(sum(x,x1))
	}
	else ROI = -1e50
	return(ROI)
}

plotgTstFn <-  function(x1,x2){
	x <- c(x1,x2)
	z <- gTstFn(x)
	return(z)
}

## wgTstFn1
## 4.08672   <= x[1] <= 110.34144
## 11.147933 <= x[2] <= 200.6628
## 8.0891    <= x[3] <= 242.673
## 6.36498   <= x[4] <= 126.3249
# This is wrong it violates the constraint
## min: f((4.16016,76.5438,8.1673,6.36498)) = 231769.0329
## use build_bwgtf
#Maximizing function changed -1e9 to 1e9 and y to -y
wgTstFn1 <- function(x){
	if(length(x) <=2){
		x <- c(x,rep(0,4-length(x)))
		#x[3] <- 153.36947
		#x[4] <- 111.81812
	}
	if (sum(6960.099222*x[1],3250.412927*x[2],21447.01471*x[3],10477.90394*x[4]) < 5e5){
		y = -exp(6.24 + log(sum(0.04862*x[1],0.07323*x[2],0.08621*x[3],0.06747*x[4])))
	}
	else y = 1e9
	return(y)
}

plotwgTstFn1 <-  function(x1,x2){
	x <- c(x1,x2)
	z <- wgTstFn1(x)
	return(z)
}

## wgTstFn2
## 3.1659525 <= x[1] <= 63.31905
## 6.49038   <= x[2] <= 207.69216
## 4.90759   <= x[3] <= 103.05939
## 5.916     <= x[4] <= 165.648
## min: f((59.56381,6.56152,4.92277,5.964)) = 303049.5018
## use build_bwgtf
#Changed for maximization
wgTstFn2 <- function(x){
	if(length(x) <=2){
		x <- c(x,rep(0,4-length(x)))
	}
	if (sum(6960.099222*x[1],3250.412927*x[2],21447.01471*x[3],10477.90394*x[4]) < 500000){
		y = -exp(6.975 + log(sum(0.08452*x[1],0.06312*x[2],0.06082*x[3],0.05668*x[4])))
	}
	else y = 1e9
	return(y)
}

plotwgTstFn2  <-  function(x1,x2){
	x <- c(x1,x2)
	z <- wgTstFn2(x)
	return(z)
}

## wgTstFn3
## 4.98774   <= x[1] <= 134.66898
## 9.1396    <= x[2] <= 182.792
## 3.2720825 <= x[3] <= 65.44165
## 1.9096    <= x[4] <= 45.8304
## min: f((39.56322,9.1792,3.28833,1.9384)) = 32811.65832
## use build_bwgtf
##Changed for maximization
wgTstFn3 <- function(x){
	if(length(x) <=2){
		x <- c(x,rep(0,4-length(x)))
	}

	if (sum(6960.099222*x[1],3250.412927*x[2],21447.01471*x[3],10477.90394*x[4]) < 500000){
		y = -exp(6.532 + log(sum(0.0846*x[1],0.04058*x[2],0.08935*x[3],0.0614*x[4])))
	}
	else y = 1e9
	return(y)
}

plotwgTstFn3  <-  function(x1,x2){
	x <- c(x1,x2)
	z <- wgTstFn3(x)
	return(z)
}

## wgTstFn4
## 10.17159  <= x[1] <= 203.4318
## 2.52831   <= x[2] <= 91.01916
## 9.14118   <= x[3] <= 246.81186
## 4.38668   <= x[4] <= 122.82704
## min: f((10.24318,51.51324,9.22354,4.44672)) = 77385.74832
## use build_bwgtf
##Changed for maximization
wgTstFn4 <- function(x){
	if(length(x) <=2){
		x <- c(x,rep(0,4-lengthresult(x)))
	}

	if (sum(6960.099222*x[1],3250.412927*x[2],21447.01471*x[3],10477.90394*x[4]) < 500000){
		y = -exp(6.036 + log(sum(0.07484*x[1],0.08281*x[2],0.04436*x[3],0.06004*x[4])))
	}
	else y = 1e9
	return(y)
}

plotwgTstFn4 <-  function(x1,x2){
	x <- c(x1,x2)
	z <- wgTstFn4(x)
	return(z)
}

## wgTstFn5
## 4.05525   <= x[1] <= 60.82875
## 6.8036267 <= x[2] <= 183.69792
## 3.19941   <= x[3] <= 38.39292
## 8.77455   <= x[4] <= 157.9419
## min: f((37.56575,6.81088,3.29882,8.8491)) = 35441.58242
## use build_bwgtf

wgTstFn5 <- function(x){
	if(length(x) <=2){
		x <- c(x,rep(0,4-length(x)))
	}

	if (sum(6960.099222*x[1],3250.412927*x[2],21447.01471*x[3],10477.90394*x[4]) < 500000){
		y = -exp(6.408 + log(sum(0.08678*x[1],0.08164*x[2],0.08269*x[3],0.07015*x[4])))
	}
	else y = 1e9
	return(y)
}

plotwgTstFn5 <-  function(x1,x2){
	x <- c(x1,x2)
	z <- wgTstFn5(x)
	return(z)
}

## wgDelFn



##############################################
## Bounds matrix builders
##############################################

build_b01 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(5.12,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b02 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(65.536,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b03 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(5.12*x,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b04 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(2.048,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b05 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(500,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b06 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(600,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b07 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(1,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b08 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(32.868,x,2)
		ret[,1] = -1*ret[,1]
		return(ret)
	}
	else return(NULL)
}

build_b09 <- function(x){
	if ((is.numeric(x)) && (floor(x) == x)){
		ret <- matrix(0,x,2)
		ret[,2] = c(rep(pi,x))
		return(ret)
	}
	else return(NULL)
}

build_b10 <- function(){
	ret <- matrix(0,2,2)
	ret[,2] = c(rep(10,2))
	return(ret)
}

build_b11 <- function(){
	ret <- matrix(0,2,2)
	ret[1,1] = -5
	ret[,2] = c(10,15)
	return(ret)
}

build_b12 <- function(){
	ret <- matrix(100,2,2)
	ret[,1] = -1*ret[,1]
	return(ret)
}

build_b13 <- function(){
	ret <- matrix(2,2,2)
	ret[,1] = -1*ret[,1]
	return(ret)
}

build_b14 <- function(){
	ret <- cbind(c(-3,-2),-1*c(-3,-2))
	return(ret)
}

build_bgtf <- function(){
	mat <- matrix(0,7,2)
	mat[,2] = c(rep(20,7))
	return(mat)
}

build_bwgtf <- function(x){
	if ((is.numeric(x)) && (floor(x) == x) && (x > 0) && (x < 6)){
		if (x == 1)	ret <- cbind(c(4.08672,11.147933,8.0891,6.36498),c(110.34144,200.6628,242.673,126.3249))
		if (x == 2)	ret <- cbind(c(3.1659525,6.49038,4.90759,5.916),c(63.31905,207.69216,103.05939,165.648))
		if (x == 3)	ret <- cbind(c(4.98774,9.1396,3.2720825,1.9096),c(134.66898,182.792,65.44165,45.8304))
		if (x == 4)	ret <- cbind(c(10.17159,2.52831,9.14118,4.38668),c(203.4318,91.01916,246.81186,122.82704))
		if (x == 5)	ret <- cbind(c(4.05525,6.8036267,3.19941,8.77455),c(60.82875,183.69792,38.39292,157.9419))
		return(ret)
	}
	else return(NULL)
}