## Code to sort a vector

#Load all the libraries
library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
int nelements = as<int>(nElements);
Rcpp::NumericVector vec(nelements);

// Fill the vector with random numbers
vec=Rcpp::runif(nelements,0,1);

//Convert to Armadillo vector
arma::vec armaVec(vec);
//sort the vector and return it
return(Rcpp::wrap(sort(armaVec)));
'

fun.Rcpp <- cxxfunction(signature(nElements="numeric"), 
                        body=src,
                        plugin="RcppArmadillo",
                        verbose=FALSE)

fun.R <- function(nElements){
  x <- runif(nElements,min=0,max=1)
  return(sort(x))
}

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

#7 million elements
elements <- 7000000

# Run the benchmarks
benchmark(
   fun.Rcpp(elements), 
   fun.R(elements),
   fun.cmpR(elements),
   fun.cmpRcpp(elements),   
   replications=10,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')




