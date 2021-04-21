

#' Confidence Interval for the Variance and the Standard Deviation of a Normal Population
#'
#' \code{variance.CI} provides a pointwise estimation and a confidence interval for the variance and the standard deviation of a normal population in both scenarios: known and unknown population mean.
#'
#' @param x a numeric vector containing the sample.
#' @param s a single numeric value corresponding with the sample standard deviation.
#' @param sc a single numeric value corresponding with the cuasi-standard deviation.
#' @param smu if known, a single numeric value corresponding with the estimation of the standard deviation for known population mean.
#' @param mu if known, a single numeric value corresponding with the population mean. Even when the user provides smu, mu is still needed.
#' @param n a single positive integer corresponding with the sample size; not needed if the sample is provided.
#' @param conf.level  a single numeric value corresponding with the confidence level of the interval; must be a value in (0,1).
#'
#' @details The formula interface is applicable when the user provides the sample and also when the user provides the value of sample characteristics (cuasi-standard deviation or sample standard deviation and the sample size).
#'
#' @return A list containing the following components:
#' \item{var.estimate}{the cuasi-variance for unknown population mean, and the estimation of the variance for known population mean.}
#' \item{sd.estimate}{the cuasi-standard deviation for unknown population mean and the estimation of the standard deviation for known population mean.}
#' \item{CI.var}{a numeric vector of length two containing the lower and upper bounds of the confidence interval for the population variance.}
#' \item{CI.sd}{a numeric vector of length two containing the lower and upper bounds of the confidence interval for the population standard deviation.}
#' Independently on the user saving those values, the function provides a summary of the result on the console.
#'
#' @examples
#' #Given the estimation of the standard deviation with known population mean
#' dat=rnorm(20,mean=2,sd=1)
#' smu=Smu(dat,mu=2)
#' variance.CI(smu=smu,mu=2,n=20,conf.level=0.95)
#'
#' #Given the sample with known population mean
#' dat=rnorm(20,mean=2,sd=1)
#' variance.CI(dat,mu=2,conf.level=0.95)
#'
#' #Given the sample with unknown population mean
#' dat=rnorm(20,mean=2,sd=1)
#' variance.CI(dat,conf.level=0.95)
#'
#' #Given the cuasi-standard deviation with unknown population mean
#' dat=rnorm(20,mean=2,sd=1)
#' variance.CI(sc=sd(dat),n=20,conf.level=0.95)
#'
#' @export
variance.CI<-function(x=NULL, s=NULL, sc=NULL, smu=NULL, mu=NULL, n=NULL, conf.level){
  if(length(conf.level)!=1){stop("'conf.level' must be a single number in (0,1)")}
  if (any(!is.finite(conf.level))){stop("'conf.level' must be a single number in (0,1)")}
  if(conf.level<=0 | conf.level>=1){stop("conf.level must be a single number in (0,1)")}

  alpha=1-conf.level

  if(is.null(mu)&is.null(smu)){#unkown mean
    if(is.null(x)&is.null(s)&is.null(sc)){stop("For unknown population mean, either 'x', 's' or 'sc' needs to be provided")}
    if(!is.null(x)){#sample provided
      if(!is.vector(x)){stop("'x' must be a numeric vector")}
      if(length(x)==1){stop("The sample needs to be at least length two")}
      if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
      if(length(x)==1){stop("The sample needs to be at least length two")}
      if(any(!is.finite(x))){stop("'x' must be a numeric vector")}

      if(!is.null(n)){warning("As the sample is provided, 'n' is omitted and computed from the sample")}
      n=length(x)
      if(!is.null(sc)){warning("As the sample is provided, 'sc' is omitted and computed from the sample")}
      if(!is.null(s)){warning("As the sample is provided, 's' is omitted and computed from the sample")}
      sc=sd(x); s=sqrt((n-1)/n)*sc;chialpha2=qchisq(alpha/2,df=n-1); chi1alpha2=qchisq(1-alpha/2,df=n-1)
      lwr=(n-1)*sc^2/chi1alpha2; upr=(n-1)*sc^2/chialpha2
    }else{#characteristic provided
      if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
      if(length(n)!=1){stop("'n' must be a single positive integer")}
      if(any(!is.finite(n))){stop("'n' must be a single positive integer")}
      if(!is.wholenumber(n)|n<=0){stop("'n' must be a single positive integer")}
      if(is.null(s)&is.null(sc)){stop("Either 's' or 'sc' needs to be provided.")}
      if(!is.null(sc)){
        if(!is.null(s)){warning("As 'sc' is provided, 's' is not used")}
        if(length(sc)!=1){stop("'sc' must be a single number")}
        if(any(!is.finite(sc))){stop("'sc' must be a single positive number")}
        if(sc<=0){stop("'sc' must be a single positive number")};s=sqrt((n-1)/n)*sc}
      if(!is.null(s)){
        if(length(s)!=1){stop("'s' must be a single positive number")}
        if(any(!is.finite(s))){stop("'s' must be a single positive number")}
        if(s<=0){stop("'s' must be a single positive number")};sc=sqrt(n/(n-1))*s}

      chialpha2=qchisq(alpha/2,df=n-1); chi1alpha2=qchisq(1-alpha/2,df=n-1);lwr=(n-1)*sc^2/chi1alpha2; upr=(n-1)*sc^2/chialpha2
    }
    var.estimate=sc^2
    sd.estimate=sc
  }else{#known mean
    if(is.null(x)&is.null(smu)){stop("For known population mean, 'x' or 'smu' needs to be provided")}
    if(!is.null(x)){#sample provided
      if(!is.vector(x)){stop("'x' must be a numeric vector")}
      if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
      if(length(x)==1){stop("The sample needs to be at least length two")}
      if(any(!is.finite(x))){stop("'x' must be a numeric vector")}
      if(is.null(smu)&is.null(mu)){stop("'mu' needs to be provided when 'smu' is not")}
      if(!is.null(n)){warning("As the sample is provided, 'n' is omitted and computed from the sample")}
      n=length(x)
      if(!is.null(mu)){
        if(length(mu)!=1){stop("'mu' must be a single number")}
        if(any(!is.finite(mu))){stop("'mu' must be a single number")}
        smu=(1/n)*sum((x-mu)^2)}
      chialpha2=qchisq(alpha/2,df=n); chi1alpha2=qchisq(1-alpha/2,df=n);lwr=n*smu^2/chi1alpha2; upr=n*smu^2/chialpha2
    }else{#smu provided
      if(length(smu)!=1){stop("'smu' must be a single positive number")}
      if(any(!is.finite(smu))){stop("'smu' must be a single positive number")}
      if(smu<=0){stop("'smu' must be a single positive number")
      if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
      if(length(n)!=1){stop("'n' must be a single positive integer")}
      if(any(!is.finite(n))){stop("'n' must be a single positive integer")}
      if(!is.wholenumber(n)|n<=0){stop("'n' must be a single positive integer")}
      }

      chialpha2=qchisq(alpha/2,df=n); chi1alpha2=qchisq(1-alpha/2,df=n);lwr=n*smu^2/chi1alpha2; upr=n*smu^2/chialpha2
    }
    if(!is.null(s)) {warning("As the population mean is known, 's' is not used")}
    if(!is.null(sc)){warning("As the population mean is known, 'sc' is not used")}
    var.estimate=smu^2
    sd.estimate=smu
 }

  CI.var=c(lwr,upr)
  CI.sd=sqrt(CI.var)

  if(is.null(mu)&is.null(smu)){
    cat(paste("\n                                      Sample variance:", round(s^2,5),"units\U00B2\n"))
    cat(paste("\n                                      Cuasi-variance:", round(sc^2,5),"units\U00B2\n"))
    cat(paste("\n           ",conf.level*100,"% confidence interval for the variance of a Normal population with unknown mean\n
                             ((n-1)S\U00B2\U0063/\U03C7\U00B2\U2099\U208B\U2081,\U2081\U208B\U003B1\U00338\U2082  ,  (n-1)S\U00B2\U0063/\U03C7\U00B2\U2099\U208B\U2081,\U003B1\U00338\U2082)\n \n",
              "                                         (",round(CI.var[1],5)," , ",round(CI.var[2],5),") units\U00B2\n\n",sep=""))
    cat(paste("\n                                Sample standard deviation:", round(s,5),"units\n"))
    cat(paste("\n                                Cuasi-standard deviation:", round(sc,5),"units\n"))
    cat(paste("\n     ",conf.level*100,"% confidence interval for the standard deviation of a Normal population with unknown mean\n
                    (sqrt((n-1)S\U00B2\U0063/\U03C7\U00B2\U2099\U208B\U2081,\U2081\U208B\U003B1\U00338\U2082)  ,  sqrt((n-1)S\U00B2\U0063/\U03C7\U00B2\U2099\U208B\U2081,\U003B1\U00338\U2082))\n \n",
              "                                     (",round(CI.sd[1],5)," , ",round(CI.sd[2],5),") units\n",sep=""))
  }else{
    cat(paste("\n                                          S\U00B2\U03BC:", round(smu^2,5),"units\U00B2\n"))
    cat(paste("\n           ",conf.level*100,"% confidence interval for the variance of a Normal population with known mean\n
                                (nS\U00B2\U03BC/\U03C7\U00B2\U2099,\U2081\U208B\U003B1\U00338\U2082  ,  nS\U00B2\U03BC/\U03C7\U00B2\U2099,\U003B1\U00338\U2082)\n \n",
              "                                       (",round(CI.var[1],5)," , ",round(CI.var[2],5),") units\U00B2\n\n",sep=""))
    cat(paste("\n                                          S\U03BC:", round(smu,5),"units\n"))
    cat(paste("\n      ",conf.level*100,"% confidence interval for the standard deviation of a Normal population with known mean\n
                           (sqrt(nS\U00B2\U03BC/\U03C7\U00B2\U2099,\U2081\U208B\U003B1\U00338\U2082)  ,  sqrt(nS\U00B2\U03BC/\U03C7\U00B2\U2099,\U003B1\U00338\U2082))\n \n",
              "                                       (",round(CI.sd[1],5)," , ",round(CI.sd[2],5),") units\n",sep=""))

  }

  invisible(list(var.estimate=var.estimate, sd.estimate=sd.estimate,CI.var=CI.var,CI.sd=CI.sd)) }







