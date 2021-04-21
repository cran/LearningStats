

#' Confidence Interval for the Mean of a Normal Population
#'
#' \code{Mean.CI} provides a pointwise estimation and a confidence interval for the mean of a Normal population in both scenarios: known and unknown population variance.
#' @param x numeric value or vector containing either the sample or the sample mean.
#' @param sigma if known, a single numeric value corresponding with the population standard deviation.
#' @param sc a single numeric value corresponding with the cuasi-standard deviation; not needed if if the sample is provided.
#' @param s a single numeric value corresponding with the sample standard deviation; not needed if the sample is provided.
#' @param n a single positive integer corresponding with the sample size; not needed if the sample is provided.
#' @param conf.level a single value corresponding with the confidence level of the interval; must be a value in (0,1).
#'
#' @details The formula interface is applicable when the user provides the sample and also when the user provides the value of the sample characteristics (sample mean, cuasi-standard deviation or sample standard deviation, jointly with the sample size).
#'
#' @return A list containing the following components:
#' \item{estimate}{a numeric value corresponding with the sample mean.}
#' \item{CI}{a numeric vector of length two containing the lower and upper bounds of the confidence interval.}
#' Independently on the user saving those values, the function provides a summary of the result on the console.
#'
#' @examples
#' #Given the sample with known population variance
#' dat=rnorm(20,mean=2,sd=1)
#' Mean.CI(dat, sigma=1, conf.level=0.95)
#'
#' #Given the sample with unknown population variance
#' dat=rnorm(20,mean=2,sd=1)
#' Mean.CI(dat, conf.level=0.95)
#'
#' #Given the sample mean with known population variance:
#' dat=rnorm(20,mean=2,sd=1)
#' Mean.CI(mean(dat),sigma=1,n=20,conf.level=0.95)
#'
#' #Given the sample mean with unknown population variance:
#' dat=rnorm(20,mean=2,sd=1)
#' Mean.CI(mean(dat),sc=sd(dat),n=20,conf.level=0.95)
#'
#' @export
Mean.CI<-function(x, sigma=NULL, sc=NULL, s=NULL, n=NULL, conf.level){
  if(!is.vector(x)){stop("'x' must be a single number or a numeric vector")}
  if(length(conf.level)!=1){stop("'conf.level' must be a single number in (0,1)")}
  if (any(!is.finite(conf.level))){stop("'conf.level' must be a single number in (0,1)")}
  if(conf.level<=0 | conf.level>=1){stop("conf.level must be a single number in (0,1)")}
  alpha=1-conf.level
  z=qnorm(1-alpha/2)


  if(length(x)>1){
    if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
    if(!length(x)>1){stop("'x' must be at least length 2")}
    if(any(!is.finite(x))){stop("'x' must be a numeric vector")}
    if(!is.null(n)){warning("As the sample is provided, 'n' value is omitted and computed from the sample")}
    n=length(x)
    if(is.null(sigma)){
      #if(!is.null(sc)){if(!is.numeric(sc)){stop("'sc' must be numeric")};if(sc==0){warning("'sc' can't be zero")}}
      #if(!is.null(s)){if(!is.numeric(s)){stop("'s' must be numeric")};if(s==0){warning("'sc' can't be zero")}}
      if(!is.null(sc)){warning("As the sample is provided, 'sc' value is omitted and computed from the sample")}
      if(!is.null(s)){warning("As the sample is provided, 's' value is omitted and computed from the sample")}
      xbar=mean(x);sc=sd(x);t=qt(1-alpha/2,df=n-1)
      lwr=xbar-t*sc/sqrt(n);upr=xbar+t*sc/sqrt(n)
    }else{
      if(length(sigma)!=1){stop("'sigma' must be a single positive number")}
      if(any(!is.finite(sigma))){stop("'sigma' must be a single positive number")}
      if(sigma<=0){stop("'sigma' must be a single positive number")}
      #if(!is.null(sc)){if(sc==0){warning("'sc' can't be zero")}}
      #if(!is.null(s)){if(s==0){warning("'sc' can't be zero")}}
      if(!is.null(sc)){warning("As 'sigma'  is provided, 'sc' value is omitted and the known variance formula is applied")}
      if(!is.null(s)){warning("As ''sigma' is provided, 's' value is omitted and the known variance formula is applied")}
      xbar=mean(x)
      lwr=xbar-z*sigma/sqrt(n);upr=xbar+z*sigma/sqrt(n)
    }
  }else if(length(x)==1){
    if(any(!is.finite(x))){stop("'x' must be a single number")}
    xbar=x
    if(is.null(sigma)){
      if(is.null(sc)&is.null(s)){stop("Either sigma' (if known), 's' or 'sc' needs to be provided")}
      if(is.null(n)){stop("The sample size needs to be provided when the sample is not")}
      if(length(n)!=1){stop("'n' must be a single positive integer")}
      if(any(!is.finite(n))){stop("'n' must be a single positive integer")}
      if(!is.wholenumber(n)|n<=0){stop("'n' must be aa single positive integer")}
      if(!is.null(sc)){
        if(!is.null(s)){warning("As 'sc' is provided, 's' is not used")}
        if(length(sc)!=1){stop("'sc' must be a single positive number")}
        if(any(!is.finite(sc))){stop("'sc' must be a single positive number")}
        if(sc<=0){stop("'sc' must be a single positive number")}
        s=sqrt((n-1)/n)*sc
      }
      if(!is.null(s)){
        if(length(s)!=1){stop("'s' must be a single positive number")}
        if(any(!is.finite(s))){stop("'s' must be a single positive number")}
        if(s<=0){stop("'s' must be a single positive number")}
        sc=sqrt(n/(n-1))*s
      }


      t=qt(1-alpha/2,df=n-1)
      lwr=xbar-t*sc/sqrt(n);upr=xbar+t*sc/sqrt(n)
    }else{
      if(length(sigma)!=1){stop("'sigma' must be a single positive number")}
      if(any(!is.finite(sigma))){stop("'sigma' must be a single positive number")}
      if(sigma<=0){stop("'sigma' must be a single positive number")}
      if(!is.null(sc)){warning("As 'sigma'  is provided, 'sc' value is omitted and the known variance formula is applied")}
      if(!is.null(s)){warning("As ''sigma' is provided, 's' value is omitted and the known variance formula is applied")}
      if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
      if(length(n)!=1){stop("'n' must be a single positive integer")}
      if(any(!is.finite(n))){stop("'n' must be a single positive integer")}
      if(!is.wholenumber(n)|n<=0){stop("'n', must be a single positive integer")}
      lwr=xbar-z*sigma/sqrt(n);upr=xbar+z*sigma/sqrt(n)
    }
  }

  CI=c(lwr,upr)



  if(is.null(sigma)){
    cat(paste("\n                                Sample mean:", round(xbar,5),"units\n"))
    cat(paste("\n        ",conf.level*100,"% confidence interval for the mean of a Normal population with unknown variance \n
                  (bar.\U0058 - t\U2099\U208B\U2081,\U2081\U208B\U03B1\U00338\U2082*Sc/\U221An  ,  bar.\U0058 + t\U2099\U208B\U2081,\U2081\U208B\U03B1\U00338\U2082*Sc/\U221An\n \n",
              "                                (",round(CI[1],5)," , ",round(CI[2],5),") units\n",sep=""))
  }else{
    cat(paste("\n                                Sample mean:", round(xbar,5),"units\n"))
    cat(paste("\n        ",conf.level*100,"% confidence interval for the mean of a Normal population with known variance\n
                    (bar.\U0058 - \U007A\U2081\U208B\U003B1\U00338\U2082 \U2217 \U03C3/\U221An  ,  bar.\U0058 + \U007A\U2081\U208B\U003B1\U00338\U2082 \U2217 \U03C3/\U221An)\n \n",
              "                                 (",round(CI[1],5)," , ",round(CI[2],5),") units\n",sep=""))

  }
  invisible(list(estimate=xbar,CI=CI))}








