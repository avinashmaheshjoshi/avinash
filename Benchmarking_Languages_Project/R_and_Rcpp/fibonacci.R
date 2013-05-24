library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
 int nnumbers = as<int>(nNumbers);
 double phi=1.6180339887498949;
 arma::vec  v = floor(1000*arma::randu<arma::vec>(nnumbers));
 
//Replace each number with its fibonacci sum
 for(int i=0;i<nnumbers;i++){
  v(i)=floor((pow(phi,v(i))-pow(phi,-v(i)))/pow(5,0.5));
 }
 return(wrap(v));
 '
 
 fun.Rcpp <- cxxfunction(body=src,
                        signature(nNumbers="numeric"),
                        plugin="RcppArmadillo",
                        verbose=FALSE)
 
 fun.R <- function(nNumbers){
   phi <- 1.6180339887498949
   a <- floor(runif(floor(nNumbers))*1000)
   return((phi^a - (-phi)^(-a))/sqrt(5)) 
 }
 
fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)
 
nNumbers=3500000 
  
benchmark(
   fun.Rcpp(nNumbers), 
   fun.R(nNumbers),
   fun.cmpR(nNumbers),
   fun.cmpRcpp(nNumbers),   
   replications=1,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')
