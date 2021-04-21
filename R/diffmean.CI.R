

#' Confidence Interval for the Difference between the Means of Two Normal Populations.
#'
#' \code{diffmean.CI} provides a pointwise estimation and a confidence interval for the difference between the means of two Normal populations in different scenarios: population variances known or unknown, population variances assumed equal or not, and paired or independent populations.
#'
#' @param x1 numeric vector or value corresponding with either one of the samples or the sample mean.
#' @param x2 numeric vector or value corresponding with either one of the samples or the sample mean.
#' @param sigma1 if known, a single numeric value corresponding with one of the population standard deviation.
#' @param sigma2 if known, a single numeric value corresponding with the other population standard deviation.
#' @param sc1 a single numeric value corresponding with the cuasi-standard deviation of one sample.
#' @param sc2 a single numeric value corresponding with the cuasi-standard deviation of the other sample.
#' @param s1 a single numeric value corresponding with the standard deviation of one sample.
#' @param s2 a single numeric value corresponding with the standard deviation of the other sample.
#' @param n1 a single positive integer value corresponding with the size of one sample; not needed if the sample is provided.
#' @param n2 a single positive integer value corresponding with the size of the other sample; not needed if the sample is provided.
#' @param paired logical value indicating whether the populations are paired or independent; default to FALSE.
#' @param var.equal logical value indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance, otherwise the Dixon and Massey approximation to the degrees of freedom is used; default to FALSE.
#' @param conf.level a single numeric value corresponding with the confidence level of the interval; must be a value in (0,1).
#'
#' @details If sigma1 and sigma2 are given, known population variances formula is applied; the unknown one is used in other case.
#'
#' If paired is TRUE then both x1 and x2 must be specified and their sample sizes must be the same. If paired is null, then it is assumed to be FALSE.
#'
#' For var.equal=TRUE, the formula of the pooled variance is \eqn{\frac{(n1-1)sc1^2+(n2-1)sc2^2}{n1+n2-2}}.
#'
#' @return A list containing the following components:
#' \item{estimate}{numeric value corresponding with the difference between the sample means.}
#' \item{CI}{a numeric vector of length two containing the lower and upper bounds of the confidence interval.}
#' Independently on the user saving those values, the function provides a summary of the result on the console.
#'
#' @examples
#' #Given unpaired samples with known population variance
#' dat1=rnorm(20,mean=2,sd=1);dat2=rnorm(30,mean=2,sd=1.5)
#' diffmean.CI(dat1,dat2,sigma1=1,sigma2=1.5,conf.level=0.9)
#'
#' #Given unpaired samples with unknown but equal population variances
#' dat1=rnorm(20,mean=2,sd=1);dat2=rnorm(30,mean=2,sd=1)
#' diffmean.CI(dat1,dat2,paired=FALSE,var.equal=TRUE,conf.level=0.9)
#'
#' #Given the characteristics of unpaired samples with unknown and different population variances
#' dat1=rnorm(20,mean=2,sd=1);dat2=rnorm(30,mean=2,sd=1)
#' x1=mean(dat1);x2=mean(dat2);sc1=sd(dat1);sc2=sd(dat2);n1=length(dat1);n2=length(dat2)
#' diffmean.CI(x1,x2,sc1=sc1,sc2=sc2,n1=n1,n2=n2,paired=FALSE,var.equal=FALSE,conf.level=0.9)
#'
#' #Given paired samples
#' dat1=rnorm(20,mean=2,sd=1);dat2=dat1+rnorm(20,mean=0,sd=0.5)
#' diffmean.CI(dat1,dat2,paired=TRUE,conf.level=0.9)
#'
#' @export
diffmean.CI<-function(x1,x2,sigma1=NULL,sigma2=NULL,sc1=NULL,sc2=NULL,s1=NULL,s2=NULL,n1=NULL,n2=NULL,paired=FALSE,var.equal=FALSE,conf.level){
  if(!is.vector(x1)){stop("'x1' must be a single number or a numeric vector")}
  if(!is.vector(x2)){stop("'x2' must be a single number or a numeric vector")}

  if(length(conf.level)!=1){stop("'conf.level' must be a single numeric value in (0,1)")}
  if (any(!is.finite(conf.level))){stop("'conf.level' must a single numeric value in (0,1)")}
  if(conf.level<=0 | conf.level>=1){stop("conf.level must be a single numeric value in (0,1)")}
  alpha=1-conf.level

  if(length(paired)!=1)stop("'paired' must be a single logical")
  if(!is.logical(paired)){stop("'paired' must be a single logical")}
  if(length(var.equal)!=1)stop("'var.equal' must be a single logical")
  if(!is.logical(var.equal)){stop("'var.equal' must be a single logical")}

  if(paired==TRUE){
    if(length(x1)==1 | length(x2)==1){stop("For paired scenario, both samples are needed")}
    iaux=complete.cases(x1,x2)
    if(sum(!iaux)!=0){ii=which(iaux==FALSE);x1=x1[-ii];x2=x2[-ii];warning("Missing values have been removed from 'x1' and 'x2'")}
    if (sum(!is.finite(x1))!=0){stop("'x1' must be a numeric vector")}
    if (sum(!is.finite(x2))!=0){stop("'x2' must be a numeric vector")}
    if(length(x1)!=length(x2)){stop("In paired populations sample sizes must be the same")}
    if(!is.null(sigma1)){warning("'sigma1' is not needed for paired samples")}
    if(!is.null(s1)){warning("'s1' is not needed for paired samples")}
    if(!is.null(sc1)){warning("'sc1' is not needed for paired samples")}
    if(!is.null(sigma2)){warning("'sigma2' is not needed for paired samples")}
    if(!is.null(s2)){warning("'s2' is not needed for paired samples")}
    if(!is.null(sc2)){warning("'sc2' is not needed for paired samples")}
    if(!is.null(n1)){warning("'n1' is not needed for paired samples")}
    if(!is.null(n2)){warning("'n2' is not needed for paired samples")}


    D=x1-x2
    n=length(D)
    Dbar=mean(D);scD=sd(D);t=qt(1-alpha/2,df=n-1)
    lwr=Dbar-t*scD/sqrt(n);upr=Dbar+t*scD/sqrt(n)
    estimate=Dbar
    CI=c(lwr,upr)
  }else if(paired==FALSE){
    if(sum(is.na(x1))!=0){x1=x1[-which(is.na(x1))]; warning("Missing values have been removed from 'x1'")}
    if(sum(is.na(x2))!=0){x2=x2[-which(is.na(x2))]; warning("Missing values have been removed from 'x2'")}
    if (any(!is.finite(x1))){stop("'x1' must be a numeric value or vector")}
    if (any(!is.finite(x2))){stop("'x2' must be a numeric value or vector")}
    if(!is.null(sigma1) | !is.null(sigma2)){#known variances
      if(is.null(sigma1) | is.null(sigma2)){stop("'sigma1' and 'sigma2' must be specified for both populations")}
      if (any(!is.finite(sigma1))){stop("'sigma1' must be numeric")}
      if(length(sigma1)!=1){stop("'sigma1' must be a single positive number")}
      if (any(!is.finite(sigma2))){stop("'sigma2' must be numeric")}
      if(length(sigma2)!=1){stop("'sigma2' must be a single positive number")}
      if(sigma1<=0){stop("'sigma1' must be a single positive number")}
      if(sigma2<=0){stop("'sigma2' must be a single positive number")}
      if(!is.null(s1)){warning("As the population variance 'sigma1' is known, 's1' is not used")}
      if(!is.null(sc1)){warning("As the population variance 'sigma1' is known, 'sc1' is not used")}
      if(!is.null(s2)){warning("As the population variance 'sigma2' is known, 's2' is not used")}
      if(!is.null(sc2)){warning("As the population variance 'sigma2' is known, 'sc2' is not used")}

      if(length(x1)>1){xbar1=mean(x1)
      if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
      n1=length(x1)
      }else{
        xbar1=x1
        if(is.null(n1)){stop("'n1' needs to be provided")}
        if(any(!is.finite(n1))){stop("'n1' must be a single positive integer")}
        if(length(n1)!=1){stop("'n1' must be a single positive integer")}
        if(!is.wholenumber(n1)|n1<=0){stop("'n1' must be a single positive integer")}
      }
      if(length(x2)>1){xbar2=mean(x2)
      if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
      n2=length(x2)
      }else{
        xbar2=x2
        if(is.null(n2)){stop("'n2' needs to be provided")}
        if(any(!is.finite(n2))){stop("'n2' must be a single positive integer")}
        if(length(n2)!=1){stop("'n2' must be a single positive integer")}
        if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}

      }
      aux=diffmean.indep.knownvar.CI(xbar1,xbar2,sigma1,sigma2,n1,n2,conf.level)
      estimate=aux$estimate
      CI=aux$CI
    }else{ #unknown variances
      # if(!is.null(sigma1)){warning("As 'sigma2' is unknown, 'sigma1' is omitted in the computation")}
      # if(!is.null(sigma2)){warning("As 'sigma1' is unknown, 'sigma2' is omitted in the computation")}
      if(var.equal==TRUE){
        if(length(x1)>1){xbar1=mean(x1)
        if(!is.null(sc1)){warning("As sample1 is provided, 'sc1' is omitted and computed from the sample")}
        if(!is.null(s1)){warning("As sample1 is provided, 's1' is omitted and computed from the sample")}
        if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
        sc1=sd(x1);n1=length(x1);s1=sqrt((n1-1)/n1)*sc1
        }else{
          xbar1=x1
          if(is.null(n1)){stop("'n1' needs to be provided")}
          if(any(!is.finite(n1))){stop("'n1' must be a single positive integer")}
          if(length(n1)!=1){stop("'n1' must be a single positive integer")}
          if(!is.wholenumber(n1)|n1<=0){stop("'n1' must be a single positive integer")}
          if(is.null(sc1)&is.null(s1)){stop("For sample1, either the sample standard deviation or the cuasi-standard deviation needs to be provided")}
          if(!is.null(sc1)){
            if(!is.null(s1)){warning("As 'sc1' is provided, 's1' is not used")}
            if(any(!is.finite(sc1))){stop("'sc1' must be a single positive number")}
            s1=sqrt((n1-1)/n1)*sc1}
          if(!is.null(s1)){
            if(any(!is.finite(s1))){stop("'s1' must be a single positive number ")}
            sc1=sqrt(n1/(n1-1))*s1}
        }
        if(length(x2)>1){xbar2=mean(x2)
        if(!is.null(sc2)){warning("As sample2 is provided, 'sc2' is omitted and computed from the sample")}
        if(!is.null(s2)){warning("As sample2 is provided, 's2' is omitted and computed from the sample")}
        if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}

        sc2=sd(x2);n2=length(x2);s2=sqrt((n2-1)/n2)*sc2
        }else{
          xbar2=x2
          if(is.null(n2)){stop("The size of sample2 needs to be provided")}
          if(any(!is.finite(n2))){stop("'n2' must be a single positive integer")}
          if(length(n2)!=1){stop("'n2' must be a single positive integer")}
          if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}
          if(is.null(sc2)&is.null(s2)){stop("For sample 2, either the sample standard deviation or the cuasi-standard deviation needs to be provided")}
          if(!is.null(sc2)){
            if(!is.null(s2)){warning("As 'sc2' is provided, 's2' is not used")}
            if(any(!is.finite(sc2))){stop("'sc2' must be a single positive number")}
            s2=sqrt((n2-1)/n2)*sc2}
          if(!is.null(s2)){
            if(any(!is.finite(s2))){stop("'s2' must be a single positive number")}
            sc2=sqrt(n2/(n2-1))*s2}
        }
        aux=diffmean.indep.unknownvareq.CI(xbar1,xbar2,sc1,sc2,n1,n2,conf.level)
        estimate=aux$estimate
        CI=aux$CI

      }else if(var.equal==FALSE){
        if(length(x1)>1){xbar1=mean(x1)
        if(!is.null(sc1)){warning("As sample1 is provided, 'sc1' is omitted and computed from the sample")}
        if(!is.null(s1)){warning("As sample1 is provided, 's1' is omitted and computed from the sample")}
        if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
        sc1=sd(x1);n1=length(x1);s1=sqrt((n1-1)/n1)*sc1
        }else{
          xbar1=x1
          if(is.null(n1)){stop("'n1' needs to be provided")}
          if(!is.numeric(n1)|!is.finite(n1)){stop("'n1' must be a single positive integer")}
          if(length(n1)!=1){stop("'n1' must be a single positive integer")}
          if(!is.wholenumber(n1)|n1<=0){stop("'n1' must be a single positive integer")}
          if(is.null(sc1)&is.null(s1)){stop("For sample1, either the sample standard deviation or the cuasi-standard deviation needs to be provided")}
          if(!is.null(sc1)){
            if(!is.null(s1)){warning("As 'sc1' is provided, 's1' is not used")}
            if(any(!is.finite(sc1))){stop("'sc1' must be a single positive number")}
            s1=sqrt((n1-1)/n1)*sc1}
          if(!is.null(s1)){if(any(!is.finite(s1))){stop("'s1' must be a single positive number")};sc1=sqrt(n1/(n1-1))*s1}
        }
        if(length(x2)>1){xbar2=mean(x2)
        if(!is.null(sc2)){warning("As sample 2 is provided, 'sc2' is omitted and computed from the sample")}
        if(!is.null(s2)){warning("As sample 2 is provided, 's2' is omitted and computed from the sample")}
        if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
        sc2=sd(x2);n2=length(x2);s2=sqrt((n2-1)/n2)*sc2
        }else{
          xbar2=x2
          if(is.null(n2)){stop("'n2' needs to be provided")}
          if(any(!is.finite(n2))){stop("'n2' must be a single positive integer")}
          if(length(n2)!=1){stop("'n2' must be a single positive integerr")}
          if(!is.wholenumber(n2)|n2<=0){stop("'n2' must be a single positive integer")}
          if(is.null(sc2)&is.null(s2)){stop("For sample 2, either the sample standard deviation or the cuasi-standard deviation needs to be provided")}
          if(!is.null(sc2)){
            if(!is.null(s2)){warning("As 'sc2' is provided, 's2' is not used")}
            if(any(!is.finite(sc2))){stop("'sc2' must be a single number")}
            s2=sqrt((n2-1)/n2)*sc2}
          if(!is.null(s2)){if(any(!is.finite(s2))){stop("'s2' must be a single number")};sc2=sqrt(n2/(n2-1))*s2}
        }
        aux=diffmean.indep.unknownvardiff.CI(xbar1,xbar2,sc1,sc2,n1,n2,conf.level)
        nu=aux$nu
        estimate=aux$estimate
        CI=aux$CI
      }
    }
  }


  if(paired==TRUE){
    cat(paste("\n                   Mean of differences estimate:", round(estimate,5)," \n"))
    cat(paste("\n ",conf.level*100,"% confidence interval for the difference of two means in paired Normal populations\n
                       bar.\U0058\U2081-bar.\U0058\U2082 \U2213 t\U2099\U0208B\U2081,\U2081\U0208B\U03B1\U00338\U2082*SD/\U221An \n \n",
              "                         (",round(CI[1],5)," , ",round(CI[2],5),") \n",sep=""))}

  if(paired==FALSE){
    if(!is.null(sigma1) & !is.null(sigma2)){#known variances
      cat(paste("\n                                     Mean of differences estimate:", round(estimate,5),"\n"))
      cat(paste("\n ",conf.level*100,"% confidence interval for the difference of two means in independent Normal populations with known variances\n
                                      bar.\U0058\U2081-bar.\U0058\U2082 \U2213 \U007A\U2081\U0208B\U003B1\U00338\U2082\U2217sqrt(\U03C3\U2081\U00B2/n\U2081 + \U03C3\U2082\U00B2/n\U2082) \n \n",
                "                                             (",round(CI[1],5)," , ",round(CI[2],5),") \n",sep=""))
    }else{ #unknown variances
      if(var.equal==TRUE){
        cat(paste("\n                                     Mean of differences estimate:", round(estimate,5),"\n"))
        cat(paste("\n ",conf.level*100,"% confidence interval for the difference of two means in independent Normal populations with same unknown variances\n
                                  bar.\U0058\U2081-bar.\U0058\U2082 \U2213 t\U2099\U2081\U0208A\U2099\U2082\U0208B\U2082,\U2081\U0208B\U03B1\U00338\U2082\U2217sqrt(S\U0077\U00B2/n\U2081 + S\U0077\U00B2/n\U2082) \n \n",
                  "                                         (",round(CI[1],5)," , ",round(CI[2],5),") \n",sep=""))
      }else if(var.equal==FALSE){
        cat(paste("\n                                     Mean of differences estimate:", round(estimate,5),"\n"))
        cat(paste("\n",conf.level*100,"% confidence interval for the difference of two means in independent Normal populations with different unknown variances\n
                                    bar.\U0058\U2081-bar.\U0058\U2082 \U2213 t\U1D65,\U2081\U0208B\U03B1\U00338\U2082\U2217sqrt(S\U2081\U00B2/n\U2081 + S\U2082\U00B2/n\U2082) \n \n",
                  "                                         (",round(CI[1],5)," , ",round(CI[2],5),") \n",sep=""))
      }
    }

  }

invisible(list(estimate=estimate,CI=CI))}


#INDEPENDENT
#Known population variances
diffmean.indep.knownvar.CI<-function(x1,x2,sigma1,sigma2,n1,n2,conf.level){
  alpha=1-conf.level
  z=qnorm(1-alpha/2)
  se=sqrt(sigma1^2/n1+sigma2^2/n2)
  lwr=(x1-x2)-z*se
  upr=(x1-x2)+z*se
  estimate=x1-x2
  CI=c(lwr,upr)
  invisible(list(estimate=estimate,CI=CI))}

#Unknown equal population variances
diffmean.indep.unknownvareq.CI<-function(x1,x2,sc1,sc2,n1,n2,conf.level){ #args: mean1, mean2, sc1, sc2, n1, n2, conf.level
  alpha=1-conf.level
  estimate=x1-x2
  sw=sqrt(((n1-1)*sc1^2+(n2-1)*sc2^2)/(n1+n2-2))
  t=qt(1-alpha/2,df=n1+n2-2)
  err=sqrt(sw^2/n1+sw^2/n2)
  lwr=estimate-t*err
  upr=estimate+t*err
  CI=c(lwr,upr)
  invisible(list(estimate=estimate,CI=CI))}

#Unknown and different population variances
diffmean.indep.unknownvardiff.CI<-function(x1,x2,sc1,sc2,n1,n2,conf.level){
  alpha=1-conf.level
  estimate=x1-x2
  nu=(sc1^2/n1+sc2^2/n2)^2/((sc1^2/n1)^2/(n1-1)+(sc2^2/n2)^2/(n2-1))-2
  nu=floor(nu)
  t=qt(1-alpha/2,df=nu)
  err=sqrt(sc1^2/n1+sc2^2/n2)
  lwr=estimate-t*err
  upr=estimate+t*err
  CI=c(lwr,upr)
  invisible(list(estimate=estimate,CI=CI,nu=nu))}

