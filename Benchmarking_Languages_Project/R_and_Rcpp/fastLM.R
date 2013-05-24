library(inline)
library(RcppArmadillo)
library(rbenchmark)
library(compiler)

src <- '
     int nrow = as<int>(nRow);
     int ncol = as<int>(nCol);
    //Create matrix with random elements
    arma::mat X = arma::randu<arma::mat>(nrow,ncol-1);
    arma::colvec  y = arma::randu<arma::colvec>(nrow);
    // fit model y ~ X
    arma::colvec coef = arma::solve(X, y);  
    // residuals
    arma::colvec resid = y - X*coef;           
    // return the coeffs and the residuals 
    return Rcpp::List::create(
        Rcpp::Named("coefficients") = coef,
        Rcpp::Named("residuals") = resid
    ) ;

'

fun.Rcpp <- cxxfunction(body=src,
                        signature(nCol="numeric",nRow="numeric"),
                        plugin="RcppArmadillo",
                        verbose=FALSE)

fun.R <- function(nrows,ncols){
  #create a random data frame
  regressionDF=as.data.frame(matrix(runif(nrows*ncols,0,1),nrow=nrows,ncol=ncols))
  # run lm() and return coefs and residuals 
  result <- lm(V1~.,data=regressionDF)
  return(list(coefficients=result$coefficients,residuals=result$residuals))
}
  

fun.cmpR <- cmpfun(fun.R)
fun.cmpRcpp <- cmpfun(fun.Rcpp)

nrows=1000
ncols=1000

benchmark(
   fun.Rcpp(nrows,ncols), 
   fun.R(nrows,ncols),
   fun.cmpR(nrows,ncols),
   fun.cmpRcpp(nrows,ncols),   
   replications=1,
   columns=c("test", "replications",
            "elapsed", "relative"),
   order='relative')
