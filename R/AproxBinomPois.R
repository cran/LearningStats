#' Illustration of the Poisson approximation to Binomial
#'
#' \code{AproxBinomPois} represents the probability mass associated with a Binomial distribution with certain parameters \code{n} and \code{p} joint with the Poisson distribution with mean equal to \code{np}.
#' Note that the Binomial distribution can be approximated by a Poisson distribution when certain conditions are met (see Details).
#'
#' @param n number of independent Bernoulli trials.
#' @param p probability of success associated with the Bernoulli trial.
#' @param xlab x-axis label; default to "x".
#' @param ylab y-axis label; default to "Probability Mass".
#' @param main an overall title for the plot; default to "Poisson approximation to Binomial distribution".
#' @param col1 a single colour associated with the Binomial probability mass function; default to "grey".
#' @param col2 a single colour associated with the Poisson probability mass function; default to "red".
#'
#' @export
#'
#' @details
#' The approximation is accurate only if one of these conditions is met:
#'
#' - \code{p} in (0,0.1), \code{n}>=30 and \code{np}<5,
#'
#' - \code{p} in (0.9,1), \code{n}>=30 and \code{n(1-p)}<5. Note that given X1 a Binomial distribution with
#' parameters \code{n} and \code{p}, and X2 a Binomial distribution with parameters \code{n}
#' and \code{1-p}, it follows that P(X1=a)=P(X2=n-a). Then, the variable X2 can be approximated to a Poisson distribution
#' with parameter \code{lambda=n(1-p)} and this Poisson distribution can be used in order to approximate the mass
#' probability function associated with X1.
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' n=50;p=0.93
#' AproxBinomPois(n,p)
#' n=100;p=0.03
#' AproxBinomPois(n,p)


AproxBinomPois<-function(n,p,xlab="x",ylab="Probability Mass",main="Poisson approximation to Binomial distribution",col1="grey",col2="red"){
  if(!is.numeric(p)|!is.vector(p)|any(!is.finite(p))) stop("The parameter 'p' must be a single number in (0,1)")
  if(length(p)!=1) stop("The parameter 'p' must be a single number in (0,1)")
  if((p>=1)|(p<=0)) stop("The parameter 'p' must be a single number in (0,1)")
  if(!is.numeric(n)|!is.vector(n)|any(!is.finite(n))) stop("The parameter 'n' must be a positive integer")
  if(length(n)!=1) stop("The parameter 'n' must be a single positive integer")
  if((n<=0)|n!=as.integer(n)) stop("The parameter 'n' must be a single positive integer")
  if(length(col1)!=1) stop("The argument 'col1' must be a single colour")
  if(col1%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col1' must be a single colour")
  if(length(col2)!=1) stop("The argument 'col2' must be a single colour")
  if(col2%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col2' must be a single colour")

  x=0:min(n,qbinom(0.9999,size=n,prob=p)+5)
  px=dbinom(x,size=n,prob=p)

  if(p>=0.1 & p<=0.9){stop("The aproximation might not be accurate because p is in (0.1,0.9)")}
  if(n<30){warning("The aproximation might not be accurate because n < 30")}
  if(p<0.1 & n>=30){if(n*p>5){warning("The aproximation might not be accurate because p < 0.1 but np > 5")}}
  if(p>0.9 & n>=30){if(n*(1-p)>5){warning("The aproximation might not be accurate because p > 0.9 but n(1-p) > 5")}}

  if(p<0.1){
    px2=dpois(x,lambda=n*p)
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col1,xlab="x",ylab="Probability Mass",main=main)
    barplot(as.table(px2),col=col2,add=TRUE,den=30,names.arg = F)
    legend("topright",c("Binomial Distribution","Poisson Distribution"),pch=15,col=c(col1,col2),bty="n")
  }

  if(p>0.9){
    px2=dpois(x,lambda=n*(1-p))
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col1,xlab="x",ylab="Probability Mass",main=main)
    barplot(as.table(px2[length(x):1]),col=col2,add=TRUE,den=30,names.arg = F)
    legend("topleft",c("Binomial Distribution","Poisson Distribution"),pch=15,col=c(col1,col2),bty="n")
  }

}
