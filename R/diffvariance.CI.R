#' Confidence Interval for the Ratio Between the Variances of Two Normal Populations
#'
#' \code{diffvariance.CI} provides a pointwise estimation and a confidence interval for the ratio of Normal population variances in both scenarios: known and unknown population mean.
#'
#' @param x1 a numeric vector containing the sample of one population.
#' @param x2 a numeric vector containing the sample of the other population.
#' @param s1 a single numeric value corresponding with the sample standard deviation of the first sample.
#' @param s2 a single numeric value corresponding with the sample standard deviation of the second sample.
#' @param sc1 a single numeric value corresponding with the cuasi-standard deviation of the first sample.
#' @param sc2 a single numeric value corresponding with the cuasi-standard deviation of the second sample.
#' @param smu1 if known, a single numeric value corresponding with the estimation of the standard deviation of the first sample.
#' @param smu2 if known, a single numeric value corresponding with the estimation of the standard deviation of the second sample.
#' @param mu1 if known, a single numeric  corresponding with the mean of one population.
#' @param mu2 if known, a single numeric value corresponding with the mean of the other population.
#' @param n1 a single positive integer corresponding with the size of one sample.
#' @param n2 a single positive integer corresponding with the size of the other sample.
#' @param conf.level is the confidence level of the interval; must be a value in (0,1).
#'
#' @details The formula interface is applicable when the user provides the sample and when the user provides the value of the sample characteristics (sample mean, cuasi-standard deviation or sample standard deviation, and sample size). Moreover, when mu1, smu1, mu2 or smu2 are provided, the function performs the procedure with known population means, and unknown in other case.
#'
#' @return A list containing the following components:
#' \item{var.estimate}{numeric value corresponding with the ratio of cuasi-variances for unknown population mean, and the ratio of the sample variances for known population mean.}
#' \item{sd.estimate}{numeric value corresponding with the ratio of cuasi-standard deviations for unknown population mean and the ratio of the sample standard deviations for known population mean.}
#' \item{CI.var}{a numeric vector of length two containing the lower and upper bounds of the confidence interval for the population variance.}
#' \item{CI.sd}{a numeric vector of length two containing the lower and upper bounds of the confidence interval for the population standard deviation.}
#' Independently on the user saving those values, the function provides a summary of the result on the console.
#'
#' @examples
#' #Given the samples with known population means
#' dat1=rnorm(20,mean=2,sd=1); dat2=rnorm(30,mean=3,sd=1)
#' diffvariance.CI(x1=dat1,x2=dat2,mu1=2,mu2=3,conf.level=0.95)
#'
#' #Given the sample standard deviations with known population means
#' dat1=rnorm(20,mean=2,sd=1); dat2=rnorm(30,mean=3,sd=1)
#' smu1=Smu(dat1,mu=2);smu2=Smu(dat2,mu=3)
#' diffvariance.CI(smu1=smu1,smu2=smu2,n1=20,n2=30,conf.level=0.95)
#'
#' #Given the samples with unknown population means
#' dat1=rnorm(20,mean=2,sd=1); dat2=rnorm(30,mean=3,sd=1)
#' diffvariance.CI(x1=dat1,x2=dat2,conf.level=0.95)
#'
#' #Given the sample standard deviations with unknown population means
#' dat1=rnorm(20,mean=2,sd=1); dat2=rnorm(30,mean=3,sd=1)
#' diffvariance.CI(s1=(19/20)*sd(dat1),s2=(29/30)*sd(dat2),n1=20,n2=30,conf.level=0.95)
#'
#' @export
diffvariance.CI<-function(x1=NULL, x2=NULL, s1=NULL, s2=NULL, sc1=NULL, sc2=NULL, smu1=NULL, smu2=NULL, mu1=NULL, mu2=NULL, n1=NULL, n2=NULL, conf.level){

  if (any(!is.finite(conf.level))){stop("'conf.level' must be a single number in (0,1)")}
  if(length(conf.level)!=1){stop("'conf.level' must be a single number in (0,1)")}
  if(conf.level<=0 | conf.level>=1){stop("conf.level must be a single number in (0,1)")}

  alpha=1-conf.level

  if(is.null(mu1)&is.null(smu1)&is.null(mu2)&is.null(smu2)){#unknown means
    if(!is.null(x1)){#sample 1 is provided
      if(!is.vector(x1)){stop("'x1' must be a numeric vector")}
      if(sum(is.na(x1))!=0){x1=x1[-which(is.na(x1))]; print("Missing values have been removed from 'x1'")}
      if (any(!is.finite(x1))){stop("'x1' must be numeric vector")}
      if(length(x1)==1){stop("Sample 1 must have at least length two")}
      if(!is.null(sc1)){warning("As sample 1 is provided, 'sc1' is omitted and computed from the sample")}
      if(!is.null(s1)){warning("As sample 1 is provided, 's1' is omitted and computed from the sample")}
      if(!is.null(n1)){warning("As sample 1 is provided, 'n1' is omitted and computed from the sample")}
      n1=length(x1);sc1=sd(x1);s1=sqrt((n1-1)/n1)*sc1
      if(!is.null(x2)){#sample 2 is provided
        if(!is.vector(x2)){stop("'x2' must be a numeric vector")}
        if(sum(is.na(x2))!=0){x2=x2[-which(is.na(x2))]; print("Missing values have been removed from 'x2'")}
        if (any(!is.finite(x2))){stop("'x2' must be a numeric vector")}
        if(length(x2)==1){stop("Sample 2 must have at least length two")}
        if(!is.null(sc2)){warning("As sample 2 is provided, 'sc2' is omitted and computed from the sample")}
        if(!is.null(s2)){warning("As sample 2 is provided, 's2' is omitted and computed from the sample")}
        if(!is.null(n2)){warning("As sample 1 is provided, 'n2' is omitted and computed from the sample")}
        n2=length(x2);sc2=sd(x2);s2=sqrt((n2-1)/n2)*sc2
      }else{#sample 2 is not provided
        if(is.null(n2)){stop("'n2' needs to be provided when the sample is not")}
        if(any(!is.finite(n2))){stop("'n2' must be a numeric vector")}
        if(length(n2)!=1){stop("'n2' must be a single positive integer")}
        if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}
        if(is.null(sc2) & is.null(s2)){stop("'s2' or 'sc2' needs to be provided when the sample is not.")}
        if(!is.null(sc2)){
          if(!is.null(s2)){warning("As 'sc2' is provided, 's2' is not used")}
          if(length(sc2)!=1){stop("'sc2' must be a single positive number")}
          if(any(!is.finite(sc2))){stop("'sc2' must be a single positive number")}
          if(sc2<=0){stop("'sc2' must be positive")};s2=sqrt((n2-1)/n2)*sc2}
        if(!is.null(s2)){
          if(length(s2)!=1){stop("'s2' must be a single positive number")}
          if(any(!is.finite(s2))){stop("'s2' must be a single positive number")}
          if(s2<=0){stop("'s2' must be a single positive number")};sc2=sqrt(n2/(n2-1))*s2}

        }
    }else{#sample 1 not provided
      if(is.null(n1)){stop("'n1' needs to be provided when the sample is not")}
      if(length(n1)!=1){stop("'n1' must be a single positive integer")}
      if(any(!is.finite(n1))){stop("'n1' must be a single positive integer")}
      if(!is.wholenumber(n1)|n1<=0){stop("'n1' must be a single positive integer")}
      if(is.null(sc1) & is.null(s1)){stop("'s1' or 'sc1' needs to be provided when the sample is not.")}
      if(!is.null(sc1)){
        if(!is.null(s1)){warning("As 'sc1' is provided, 's1' is not used")}
        if(length(sc1)!=1){stop("'sc1' must be a single positive number")}
        if(any(!is.finite(sc1))){stop("'sc1' must be a single positive number")}
        if(sc1<=0){stop("'sc1' must be a single positive number")};s1=sqrt((n1-1)/n1)*sc1}
      if(!is.null(s1)){
        if(length(s1)!=1){stop("'s1' must be a single positive number")}
        if(any(!is.finite(s1))){stop("'s1' must be a single positive number")}
        if(s1<=0){stop("'s1' must be a single positive number")};sc1=sqrt(n1/(n1-1))*s1}
      if(!is.null(x2)){#sample 2 is provided
        if(!is.vector(x2)){stop("'x2' must be a numeric vector")}
        if(sum(is.na(x2))!=0){x2=x2[-which(is.na(x2))]; print("Missing values have been removed from 'x2'")}
        if (any(!is.finite(x2))){stop("'x2' must be a single positive integer")}
        if(length(x2)==1){stop("Sample 2 must have at least length two")}
        if(!is.null(sc2)){warning("As sample 2 is provided, 'sc2' is omitted and computed from the sample")}
        if(!is.null(s2)){warning("As sample 2 is provided, 's2' is omitted and computed from the sample")}
        if(!is.null(n2)){warning("As sample 2 is provided, 'n2' is omitted and computed from the sample")}
        n2=length(x2);sc2=sd(x2);s2=sqrt((n2-1)/n2)*sc2
      }else{#sample 2 is not provided
        if(is.null(n2)){stop("'n2' needs to be provided when the sample is not")}
        if(length(n2)!=1){stop("'n2' must be a single positive integer")}
        if(any(!is.finite(n2))){stop("'n2' must be a single positive integer")}
        if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}

        if(is.null(sc2) & is.null(s2)){stop("'s2' or 'sc2' needs to be provided when the sample is not.")}
        if(!is.null(sc2)){
          if(!is.null(s2)){warning("As 'sc2' is provided, 's2' is not used")}
          if(length(sc2)!=1){stop("'sc2' must be a single positive number")}
          if(any(!is.finite(sc2))){stop("'sc2' must be a single positive number")}
          if(sc2<=0){stop("'sc2' must be a single positive number")};s2=sqrt((n2-1)/n2)*sc2}
        if(!is.null(s2)){
          if(length(s2)!=1){stop("'s2' must be a single positive number")}
          if(any(!is.finite(s2))){stop("'s2' must be a single positive number")}
          if(s2<=0){stop("'s2' must be a single positive number")};sc2=sqrt(n2/(n2-1))*s2}


        }
    }
    fmin=qf(alpha/2,df1=n1-1,df2=n2-1)
    fmax=qf(1-alpha/2,df1=n1-1,df2=n2-1)
    lwr=(sc1^2/sc2^2)/fmax; upr=(sc1^2/sc2^2)/fmin
    s1=sqrt((n1-1)/n1)*sc1;s2=sqrt((n2-1)/n2)*sc2
    var.estimate=sc1^2/sc2^2
    sd.estimate=sc1/sc2
    CI.var=c(lwr,upr)
    CI.sd=sqrt(c(lwr,upr))

  }else{#known means
    if(!is.null(mu1)|!is.null(smu1)){if(is.null(mu2)&is.null(smu2)) stop("'mu' or 'smu' must be specified for both populations")}
    if(!is.null(mu2)|!is.null(smu2)){if(is.null(mu1)&is.null(smu1)) stop("'mu' or 'smu' must be specified for both populations")}

    if(!is.null(x1)){#sample 1 is provided
      if(sum(is.na(x1))!=0){x1=x1[-which(is.na(x1))]; warning("Missing values have been removed from 'x1'")}
      if (any(!is.finite(x1))){stop("'x1' must be a numeric vector")}
      if(length(x1)==1){stop("Sample 1 must have at least length two")}
      if(!is.null(n1)){warning("As sample 1 is provided, 'n1' is omitted and computed from the sample")}
      n1=length(x1)
      if(!is.null(smu1)){
        if(!is.null(mu1)){warning("As 'smu1' is provided, 'mu1' is not needed")}
        if(any(!is.finite(smu1))){stop("'smu1' must be a single positive number")}
        if(smu1<=0){stop("'smu1' must be a single positive number")}
      }else if(is.null(smu1)){
        if(is.null(mu1)){stop("'mu' or 'smu' must be specified for both populations")}
        if(length(mu1)!=1){stop("'mu1' must be a single positive number")}
        if(any(!is.finite(mu1))){stop("'mu1' must be a single positive number")}
        smu1=sum((x1-mu1)^2)/n1
      }

      if(!is.null(x2)){#sample 2 is provided
        if(sum(is.na(x2))!=0){x2=x2[-which(is.na(x2))]; warning("Missing values have been removed from 'x2'")}
        if (any(!is.finite(x2))){stop("'x2' must be a numeric vector")}
        if(length(x2)==1){stop("Sample 2 must have at least length two")}
        if(!is.null(n2)){warning("As sample 2 is provided, 'n2' is omitted and computed from the sample")}
        n2=length(x2)
        if(!is.null(smu2)){
          if(!is.null(mu2)){warning("As 'smu2' is provided, 'mu2' is not needed")}
          if(any(!is.finite(smu2))){stop("'smu2' must be a single positive number")}
          if(length(smu2)!=1){stop("'smu2' must be a single positive number")}
          if(smu2<=0){stop("'smu2' must be a single positive number")}
        }else if(is.null(smu2)){
          if(is.null(mu2)){stop("'mu' or 'smu' must be specified for both populations")}
          if(length(mu2)!=1){stop("'mu2' must be a single positive number")}
          if(any(!is.finite(mu2))){stop("'mu2' must be a single positive number")}
          smu2=sum((x2-mu2)^2)/n2
        }
      }else{#sample 2 is not provided
        if(is.null(n2)){stop("'n2' needs to be provided when the sample is not")}
        if(length(n2)!=1){stop("'n2' must be a single positive integer")}
        if(any(!is.finite(n2))){stop("'n2' must be a single positive integer")}
        if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}
        if(is.null(smu2)){stop("'smu2' needs to be provided when the sample is not")}
        if(length(smu2)!=1){stop("'smu2' must be a single positive number")};
        if(any(!is.finite(smu2))){stop("'smu2' must be a single positive number")}
        if(smu2<=0){stop("'smu2' must be a single positive number")}
        if(!is.null(mu2)&!is.null(smu2)) warning("As 'smu2' is provided, 'mu2' is not needed")
      }
    }else{#sample 1 not provided
      if(is.null(n1)){stop("'n1' needs to be provided when the sample is not")}
      if(length(n1)!=1){stop("'n1' must be a single positive integer")}
      if(any(!is.finite(n1))){stop("'n1' must be a single positive integer")}
      if(!is.wholenumber(n1)|n1<=0){stop("'n1' must be a single positive integer")}
      if(is.null(smu1)){stop("'smu1' needs to be provided when the sample is not.")}
      if(length(smu1)!=1){stop("'smu1' must be a single positive number")}
      if(any(!is.finite(smu1))){stop("'smu1' must be a single positive number")}
      if(smu1<=0){stop("'smu1' must be a single positive number")}
      if(!is.null(mu1)&!is.null(smu1)) warning("As 'smu1' is provided, 'mu1' is not needed")
      if(!is.null(x2)){#sample 2 is provided
        if(sum(is.na(x2))!=0){x2=x2[-which(is.na(x2))]; warning("Missing values have been removed from 'x2'")}
        if (any(!is.finite(x2))){stop("'x2' must be a numeric vector")}
        if(length(x2)==1){stop("Sample 2 must have at least length two")}
        if(!is.null(n2)){warning("As sample 2 is provided, 'n2' is omitted and computed from the sample")}
        n2=length(x2)
        if(!is.null(smu2)){
          if(!is.null(mu2)){warning("As 'smu2' is provided, 'mu2' is not needed")}
          if(any(!is.finite(smu2))){stop("'smu2' must be a single positive number")}
          if(length(smu2)!=1){stop("'smu2' must be a single positive number")}
          if(smu2<=0){stop("'smu2' must be a single positive number")}
        }else if(is.null(smu2)){
          if(is.null(mu2)){stop("'mu' or 'smu' must be specified for both populations")}
          if(length(mu2)!=1){stop("'mu2' must be a single positive number")}
          if(any(!is.finite(mu2))){stop("'mu2' must be a single positive number")}
          smu2=sum((x2-mu2)^2)/n2
        }
      }else{#sample 2 is not provided
        if(is.null(n2)){stop("'n2' needs to be provided when the sample is not")}
        if(length(n2)!=1){stop("'n2' must be a single positive integer")}
        if(any(!is.finite(n2))){stop("'n2' must be a single positive integer")}
        if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}
        if(is.null(smu2)){stop("'smu2' needs to be provided when the sample is not.")}
        if(length(smu2)!=1){stop("'smu2' must be a single positive number")}
        if(any(!is.finite(smu2))){stop("'smu2' must be a single positive number")}
        if(smu2<=0){stop("'smu2' must be a single positive number")}
        if(!is.null(mu2)&!is.null(smu2)) warning("As 'smu2' is provided, 'mu2' is not needed")
      }
    }
    fmin=qf(alpha/2,df1=n1,df2=n2)
    fmax=qf(1-alpha/2,df1=n1,df2=n2)
    lwr=(smu1^2/smu2^2)/fmax; upr=(smu1^2/smu2^2)/fmin
    var.estimate=smu1^2/smu2^2
    sd.estimate=smu1/smu2
    CI.var=c(lwr,upr)
    CI.sd=sqrt(c(lwr,upr))
  }

  if(is.null(mu1)&is.null(smu1)&is.null(mu2)&is.null(smu2)){
    cat(paste("\n                                      Ratio of sample variances:", round(s1^2/s2^2,5)," \n"))
    cat(paste("\n                                      Ration of cuasi-variances:", round(sc1^2/sc2^2,5)," \n"))
    cat(paste("\n           ",conf.level*100,"% confidence interval for the ratio of population variances with unknown population means\n
                           ((S\U00B2\U0063\U2081/S\U00B2\U0063\U2082)/f\U2099\U2081\U208B\U2081,\U2099\U2082\U208B\U2081,\U2081\U208B\U003B1\U00338\U2082  ,  (S\U00B2\U0063\U2081/S\U00B2\U0063\U2082)/f\U2099\U2081\U208B\U2081,\U2099\U2082\U208B\U2081,\U003B1\U00338\U2082)\n \n",
              "                                         (",round(CI.var[1],5)," , ",round(CI.var[2],5),") \n\n",sep=""))
    cat(paste("\n                                Ratio of sample standard deviations:", round(s1/s2,5)," \n"))
    cat(paste("\n                                Ratio of cuasi-standard deviations:", round(sc1/sc2,5)," \n"))
    cat(paste("\n     ",conf.level*100,"% confidence interval for the ratio of population standard deviations with unknown population means\n
                         (sqrt((S\U00B2\U0063\U2081/S\U00B2\U0063\U2082)/f\U2099\U2081\U208B\U2081,\U2099\U2082\U208B\U2081,\U2081\U208B\U003B1\U00338\U2082)  ,  sqrt((S\U00B2\U0063\U2081/S\U00B2\U0063\U2082)/f\U2099\U2081\U208B\U2081,\U2099\U2082\U208B\U2081,\U003B1\U00338\U2082))\n \n",
              "                                       (",round(CI.sd[1],5)," , ",round(CI.sd[2],5),") \n",sep=""))
  }else{
    cat(paste("\n                                          S\U00B2\U03BC\U2081/S\U00B2\U03BC\U2082:", round(smu1^2/smu2^2,5),"\n"))
    cat(paste("\n     ",conf.level*100,"% confidence interval for the ratio of population variances with known population means\n
                             ((S\U00B2\U03BC\U2081/S\U00B2\U03BC\U2082)/f\U2099\U2081,\U2099\U2082,\U2081\U208B\U003B1\U00338\U2082 , (S\U00B2\U03BC\U2081/S\U00B2\U03BC\U2082)/f\U2099\U2081,\U2099\U2082,\U003B1\U00338\U2082)\n \n",
              "                                           (",round(CI.var[1],5)," , ",round(CI.var[2],5),") \n\n",sep=""))

    cat(paste("\n                                            S\U03BC\U2081/S\U03BC\U2082:", round(smu1/smu2,5),"\n"))
    cat(paste("\n      ",conf.level*100,"% confidence interval for the ratio of population standard deviations with known population means\n
                            (sqrt((S\U00B2\U03BC\U2081/S\U00B2\U03BC\U2082)/f\U2099\U2081,\U2099\U2082,\U003B1\U00338\U2082) , sqrt((S\U00B2\U03BC\U2081/S\U00B2\U03BC\U2082)/f\U2099\U2081,\U2099\U2082,\U2081\U208B\U003B1\U00338\U2082))\n \n",
              "                                             (",round(CI.sd[1],5)," , ",round(CI.sd[2],5),") \n",sep=""))


  }



    invisible(list(var.estimate=var.estimate, sd.estimate=sd.estimate,CI.var=CI.var,CI.sd=CI.sd))}


