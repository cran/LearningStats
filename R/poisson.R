
#' Probability Mass and/or Distribution Function Representations associated with a Poisson Distribution
#'
#' \code{plotPois} represents the probability mass and/or the distribution function associated with a Poisson
#'  distribution with parameter \code{lambda}.
#'
#' @param lambda mean of the Poisson distribution.
#' @param type 	a character string giving the type of desired plot. The following values are possible:
#' "b" (default) for probability mass function and distribution function representations together,
#' "d" for distribution function representation and "p" for probability mass function representation.
#' @param col a single colour associated with the probability mass function representation; default to "grey".
#'
#' @return A matrix containing the probability mass and the distribution function associated with each point
#' of the support of a Poisson distribution with parameter \code{lambda}.
#' @export
#'
#' @examples
#' lambda=2
#' plotPois(lambda,type="d")
#' plotPois(lambda,type="p",col="pink")
#' plotPois(lambda)

plotPois<-function(lambda,type="b",col="grey"){
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
  old <- options() ; on.exit(options(old))

  options(scipen=999)
  if(!is.numeric(lambda)|any(!is.finite(lambda))) stop("The parameter 'lambda' must be a positive number")
  if(!is.vector(lambda)|length(lambda)!=1) stop("The parameter 'lambda' must be a positive number")
  if(lambda<=0) stop("The parameter 'lambda' must be a positive number")
  if(!type%in%c("b","d","p")) stop("The argument 'type' must be 'b', 'd' or 'p'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  x=0:(qpois(0.999,lambda)+2)
  px=dpois(x,lambda=lambda)
  Fx=ppois(x,lambda=lambda)
  if(is.null(type)){type="b"}

  if(type=="b"){
    par(mfrow=c(1,2))
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( Pois(lambda) = x)",main="Probability Mass")

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
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( Pois(lambda) = x)",main="Probability Mass")
  }
  res=round(as.matrix(cbind(x,px,Fx)),5)
  dimnames(res) <-list(rep("", dim(res)[1]), c("x","P(Pois(lambda)=x)","P(Pois(lambda)<=x)"))

  cat("Probability mass and distribution function associated with a Pois(lambda)\n \n")
  return(res)
}
