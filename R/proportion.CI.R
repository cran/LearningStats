

#' Large Sample Confidence Interval for a Population Proportion
#'
#' \code{proportion.CI} provides a pointwise estimation and a confidence interval for a population proportion.
#'
#' @param x a single numeric value corresponding with either the proportion estimate or the number of successes of the sample.
#' @param n a single positive integer corresponding with the sample size.
#' @param conf.level a single numeric value corresponding with the confidence level of the interval; must be a value in (0,1).
#'
#' @details Counts of successes and failures must be nonnegative and hence not greater than the corresponding numbers of trials which must be positive. All finite counts should be integers.
#'
#' If the number of successes are given, then the proportion estimate is computed.
#'
#' @return A list containing the following components:
#' \item{estimate}{numeric value corresponding with the sample proportion estimate.}
#' \item{CI}{a numeric vector of length two containing the lower and upper bounds of the confidence interval.}
#' Independently on the user saving those values, the function provides a summary of the result on the console.
#'
#' @examples
#' #Given the sample proportion estimate
#' proportion.CI(0.3, 100, conf.level=0.95)
#'
#' #Given the number of successes
#' proportion.CI(30,100,conf.level=0.95)
#'
#' @export
proportion.CI<-function(x,n,conf.level){
if(length(x)!=1){stop("'x' must be a single positive number")}
if (any(!is.finite(x))){stop("'x' must be a single positive number")}
if(x<=0){stop("x must be a single positive number")}
if(length(n)!=1){stop("'n' must be a single positive integer")}
if (any(!is.finite(n))){stop("'n' must be a singke positive integer")}
if(!is.wholenumber(n)|n<=0){stop("'n' must be a single positive integer")}

if(is.null(conf.level)){stop("'conf.level' needs to be provided")}
if(length(conf.level)!=1){stop("'conf.level' must be a single number in (0,1)")}
if (any(!is.finite(conf.level))){stop("'conf.level' must be a single number in (0,1)")}
if(conf.level<=0 | conf.level>=1){stop("'conf.level' must be a single number in (0,1)")}

if(n<=30){warning("The sample size is small for the approximation used in the method, it should be n > 30")}
if(x<1){
hatp=x}else
{
if(!is.wholenumber(x)){stop("x must be either the proportion estimate or the number of successes")}
if(x>n){stop("number of successes can't be greater than the number of trials")}
hatp=x/n
}
alpha=1-conf.level
z=qnorm(1-alpha/2)
se=sqrt(hatp*(1-hatp)/n)
CI=c(hatp-z*se,hatp+z*se)


if(x<1){
  cat(paste("\n                                Proportion estimate:", round(hatp,5),"\n"))
  cat(paste("\n                       ",conf.level*100,"% confidence interval for a proportion \n
      (hat.p - z\U2081\U208B\U03B1\U00338\U2082*sqrt(hat.p*(1-hat.p)/n)  ,  hat.p + z\U2081\U208B\U03B1\U00338\U2082*sqrt(hat.p*(1-hat.p)/n))\n \n",
            "                                  (",round(CI[1],5)," , ",round(CI[2],5),")\n",sep=""))
}else{
  cat(paste("\n                                Proportion estimate:", round(hatp,5),"\n"))
  cat(paste("\n                       ",conf.level*100,"% confidence interval for a proportion \n
       (hat.p - z\U2081\U208B\U03B1\U00338\U2082*sqrt(hat.p*(1-hat.p)/n)  ,  hat.p + z\U2081\U208B\U03B1\U00338\U2082*sqrt(hat.p*(1-hat.p)/n)\n \n",
            "                                  (",round(CI[1],5)," , ",round(CI[2],5),")\n",sep=""))

}
invisible(list(estimate=hatp,CI=CI))}






