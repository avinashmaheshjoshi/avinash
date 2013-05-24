library(inline)
library(Rcpp)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
 int nrow = as<int>(nRow);
 int ncol = as<int>(nCol);
 
//Create matrix with random elements
//arma::mat M = arma::randu<arma::mat>(nrow,ncol);

//Create matrix with 0 mean unit variance
arma::mat M = arma::randn<arma::mat>(nrow,ncol);

//Uncomment if you just want to create a matrix
//return(Rcpp::wrap(M));

// Now return the transpose
//return(Rcpp::wrap(trans(M)));

//Code for resizing the matrices
//arma::mat resizeM(M.begin(), nrow/2, ncol*2, false); // reuses memory and avoids extra copy
//return(Rcpp::wrap(resizeM));

//Code to compute the cross product
//return(Rcpp::wrap(trans(M)*M));

//take the 1000th power of the matrix
return(Rcpp::wrap(pow(M,1000)));

' 

fun.Rcpp <- cxxfunction(signature(nCol="numeric",nRow="numeric"), body=src, plugin="RcppArmadillo",verbose=FALSE)

fun.R <- function(nRow,nCol){
 #Create a random vector
 #M=matrix(runif(nRow*nCol,0,1),nrow=nRow,ncol=nCol)
 
 #Create a normal vector with 0 mean and unit variance
 M=matrix(rnorm(nRow*nCol,0,1),nrow=nRow,ncol=nCol)
 
 #Uncomment if you just want to create a matrix
 #return(M)
 
 #Now return the transpose
 #return(t(M))
 
 #resize the matrix
 #M=matrix(as.vector(M),nrow=nRow/2,ncol=nCol*2)
 #return(M)
 
 #compute the cross product
 #return(t(M)%*%M)

 
 #Take the 1000th power of each element
 return(M^1000)
}

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

dimension <- 500

benchmark(
   fun.Rcpp(dimension,dimension), 
   fun.R(dimension,dimension),
   fun.cmpR(dimension,dimension),
   fun.cmpRcpp(dimension,dimension),	 
   replications=100,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')
