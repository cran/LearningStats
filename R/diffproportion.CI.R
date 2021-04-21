

#' Large Sample Confidence Interval for the Difference between Two Population Proportions
#'
#' \code{diffproportion.CI} provides a pointwise estimation and a confidence interval for the difference between two population proportions.
#'
#' @param x1 a single numeric value corresponding with either the proportion estimate or the number of successes of one of the samples.
#' @param x2 a single numeric value corresponding with either the proportion estimate or the number of successes of the other sample.
#' @param n1 a single positive integer value corresponding with one sample size.
#' @param n2 a single positive integer value corresponding with the other sample size.
#' @param conf.level a single numeric value corresponding with the confidence level of the interval; must be a value in (0,1).
#'
#' @details Counts of successes and failures must be nonnegative and hence not greater than the corresponding numbers of trials which must be positive. All finite counts should be integers.
#' If the number of successes are given, then the proportion estimate is computed.
#'
#' @return A list containing the following components:
#' \item{estimate}{a numeric value corresponding with the difference between the two sample proportions.}
#' \item{CI}{a numeric vector of length two containing the lower and upper bounds of the confidence interval.}
#' Independently on the user saving those values, the function provides a summary of the result on the console.
#'
#' @examples
#' #Given the sample proportion estimate
#' diffproportion.CI(0.3,0.4,100,120,conf.level=0.95)
#'
#' #Given the number of successes
#' diffproportion.CI(30,48,100,120,conf.level=0.95)
#'
#' #Given in one sample the number of successes and in the other the proportion estimate
#' diffproportion.CI(0.3,48,100,120,conf.level=0.95)
#'
#' @export
diffproportion.CI<-function(x1,x2,n1,n2,conf.level){
  if(is.null(x1)|is.null(x2)){stop("Either the proportion estimate or the number of successes needs to be provided for both samples.")}
  if(is.null(n1)|is.null(n2)){stop("Both sample sizes need to be provided")}

  if(length(x1)!=1){stop("'x1' must be a single positive number")}
  if (any(!is.finite(x1))){stop("'x1' must be a single positive number")}

  if(length(x2)!=1){stop("'x2' must be a single positive number")}
  if (any(!is.finite(x2))){stop("'x2' must be a single positive number")}

  if(length(n1)!=1){stop("'n1' must be a single positive number")}
  if (any(!is.finite(n1))){stop("'n1' must be a single positive integer")}

  if(length(n2)!=1){stop("'n2' must be a single positive number")}
  if (any(!is.finite(n2))){stop("'n2' must be a single positive integer")}

  if(!is.wholenumber(n1)|n1<=0){stop("'n1' must be a positive integer")}
  if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a positive integer")}

  if (any(!is.finite(conf.level))){stop("'conf.level' must be a single number in (0,1)")}
  if(length(conf.level)!=1){stop("'conf.level' must be a single number in (0,1)")}
  if(conf.level<=0 | conf.level>=1){stop("conf.level must be a single number in (0,1) ")}


  if(n1<=30|n2<=30){warning("The sample size is small for approximation used in the method, it should be n > 30")}
  if(x1<=0){stop("'x1' can't be negative, it must be either a proportion estimate in (0,1) or the number of successes")}
  if(x2<=0){stop("'x2' can't be negative, it must be either a proportion estimate in (0,1) or the number of successes")}
  if(x1<1){hatp1=x1}
  if(x2<1){hatp2=x2}
  if(x1>=1){
    if(!is.wholenumber(x1)|x1<=0){stop("'x1' must be either the proportion estimate in (0,1) or the number of successes")}
    if(x1>n1){stop("The number of successes 'x1' can't be greater than the number of trials 'n1'")}
    hatp1=x1/n1
  }

  if(x2>=1){
    if(!is.wholenumber(x2)|x2<=0){stop("'x2' must be either the proportion estimate in (0,1) or the number of successes")}
    if(x2>n2){stop("The number of successes 'x2' can't be greater than the number of trials 'n2'")}
    hatp2=x2/n2
  }

  alpha=1-conf.level
  z=qnorm(1-alpha/2)
  pw=(n1*hatp1+n2*hatp2)/(n1+n2)
  se=sqrt(pw*(1-pw)*(1/n1+1/n2))
  estimate=hatp1-hatp2
  CI=c(estimate-z*se,estimate+z*se)

    cat(paste("\n                          Difference of proportions estimate:", round(estimate,5),"\n"))
    cat(paste("\n                       ",conf.level*100,"% confidence interval for a proportion difference\n
          hat.p\U2081-hat.p\U2082 \U2213 z\U2081\U208B\U03B1\U00338\U2082*sqrt(pw*(1-pw)*(1/n\U2081+1/n\U2082)); where pw=(n\U2081hat.p\U2081+n\U2082hat.p\U2082)/(n\U2081+n\U2082)\n \n",
              "                                   (",round(CI[1],5)," , ",round(CI[2],5),")\n",sep=""))

  invisible(list(estimate=(hatp1-hatp2),CI=CI))}







