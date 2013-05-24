## Code to perform simulated annealing
rm(list=ls())
#Load all the libraries
library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

#Objective function - Ackleys 2d function
objFnc <-' double ackF(int nParams,arma::colvec x){
    double pi=3.141569;
    double outVal=(10*nParams)+arma::sum((-10*arma::cos(2*pi*x))+arma::square(x));
	  //double outVal= arma::sum(x);
    return(outVal);
}

// Generate a random number between min and max - random search
arma::colvec neighbourhood(double min,double max,int nParams){
   arma::colvec outVec = min+((max-min)*arma::randu<arma::colvec>(nParams));
   return(outVec);
}

'

#SA algorithm code
src <-'
  int nParams = as<int>(nparams);
  int min=as<int>(minimum);
  int max=as<int>(maximum);
  double cooling_sched = 0.999;
	double maxIters = 1e+20;
	double stopTemp = 1e-150;
	double eMin = -1e100;
  
  //Create a vector of nparam random elements
  arma::colvec  xVec = min+((max-min)*arma::randu<arma::colvec>(nParams));
  double eBest = ackF(nParams,xVec);
  double e = eBest;
	int k=1;
	double T=1;

  while (k < maxIters && e >= eMin && T > stopTemp){
		arma::colvec newX = neighbourhood(min,max,nParams);
		double eNew = ackF(nParams,newX);
		if(eNew < eBest){ 
      eBest = eNew;
      e = eNew;
      xVec = newX;
		}
    else {
        	if(arma::randu() < exp((e-eNew)/(k*T) )){
        		eBest = eNew;
        		e = eNew;
		    		xVec = newX;
        	}
		}

	  k = k+1;
	  T = cooling_sched*T;
	}
  //arma::colvec  test(2);
  //test(0)=5;
  //test(1)=-5;
  //return(Rcpp::wrap(ackF(2,test)));
    return Rcpp::List::create(
        Rcpp::Named("Vals") = xVec,
        Rcpp::Named("Energy") = eBest
    ) ;

'
fun.Rcpp <- cxxfunction(signature(nparams="numeric",minimum="numeric",maximum='numeric'), 
                        body=src,
                        include=objFnc,
                        plugin="RcppArmadillo",
                        verbose=FALSE)