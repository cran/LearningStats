
#' Probability Mass and/or Distribution Function Representations associated with a Negative Binomial Distribution
#'
#' \code{plotNegBinom} represents the probability mass and/or the distribution function associated with a Negative Binomial
#'  distribution with certain parameters \code{n} and \code{p}.
#'
#' @param n the number of successful Bernoulli trials.
#' @param p the probability of success associated with the Bernoulli trial.
#' @param type 	a character string giving the type of desired plot. The following values are possible:
#' "b" (default) for probability mass function and distribution function representations together,
#' "d" for distribution function representation and "p" for probability mass function representation.
#' @param col a single colour associated with the probability mass function representation; default to "grey".
#'
#' @return A matrix containing the probability mass and the distribution function associated with each point
#' of the support of a Negative Binomial distribution with parameters \code{n} and \code{p}.
#' @export
#'
#' @details Note that if \code{n=1}, the Negative Binomial distribution is also known as Geometric distribution.
#'
#' @examples
#' n=3;p=0.3
#' plotNegBinom(n,p,type="d")
#' plotNegBinom(n,p,type="p",col="pink")
#' plotNegBinom(n,p)

plotNegBinom<-function(n,p,type="b",col="grey"){
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
  old <- options() ; on.exit(options(old))

  options(scipen=999)
  if(!is.numeric(p)|!is.vector(p)|any(!is.finite(p))) stop("The parameter 'p' must be a single number between 0 and 1")
  if(length(p)!=1) stop("The parameter 'p' must be a single number between 0 and 1")
  if((p>=1)|(p<=0)) stop("The parameter 'p' must be a single number between 0 and 1")
  if(!is.numeric(n)|!is.vector(n)|any(!is.finite(n))) stop("The parameter 'n' must be a single positive integer")
  if(length(n)!=1) stop("The parameter 'n' must be a single positive integer")
  if((n<=0)|n!=as.integer(n)) stop("The parameter 'n' must be a single positive integer")
  if(!type%in%c("b","d","p")) stop("The argument 'type' must be 'b', 'd' or 'p'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")


  x=0:(qnbinom(0.999,size=n,prob=p)+2)
  px=dnbinom(x,size=n,prob=p)
  Fx=pnbinom(x,size=n,prob=p)
  if(is.null(type)){type="b"}

  if(type=="b"){
    par(mfrow=c(1,2))
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( NBi(n,p) = x)",main="Probability Mass")

    plot(c(-1,x),c(0,Fx),type="s",lty=2,lwd=2,ylab="F(x)",xlab="x",main="Distribution Function",xaxt="n")
    points(x,Fx,pch=19,cex=1.2)
    segments(c(-1,x),c(0,Fx),c(0,x[2:length(x)],max(x)+1),c(0,Fx),lwd=2)
    axis(1,at=x,labels=x,las=1)
    abline(h=c(0,1),lty=2,col="grey")
  }else if(type=="d"){
    plot(c(-1,x),c(0,Fx),type="s",lty=2,lwd=2,ylab="F(x)",xlab="x",main="Distribution Function",xaxt="n")
    points(x,Fx,pch=19,cex=1.2)
    segments(c(-1,x),c(0,Fx),c(0,x[2:length(x)],max(x)+1),c(0,Fx),lwd=2)
    axis(1,at=x,labels=x,las=1)
    abline(h=c(0,1),lty=2,col="grey")
  }else if(type=="p"){
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( NBi(n,p) = x)",main="Probability Mass")
  }
  res=round(as.matrix(cbind(x,px,Fx)),5)
  dimnames(res) <-list(rep("", dim(res)[1]), c("x","P(NBi(n,p)=x)","P(NBi(n,p)<=x)"))

  cat("Probability mass and distribution function associated with a NBi(n,p)\n \n")
  return(res)
}
