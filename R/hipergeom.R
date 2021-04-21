
#' Probability Mass and/or Distribution Function Representations associated with a Hypergeometric Distribution
#'
#' \code{plotHyper} represents the probability mass and/or the distribution function associated with a Hypergeometric
#'  distribution with parameters \code{N}, \code{n} and \code{k}.
#'
#' @param N the population size.
#' @param n the number of draws.
#' @param k the number of success states in the population.
#' @param type 	a character string giving the type of desired plot. The following values are possible:
#' "b" (default) for probability mass function and distribution function representations together,
#' "d" for distribution function representation and "p" for probability mass function representation.
#' @param col a single colour associated with the probability mass function representation; default to "grey".
#'
#' @return A matrix containing the probability mass and the distribution function associated with each point
#' of the support of a Hypergeometric distribution with parameters \code{N}, \code{n} and \code{k}.
#' @export
#'
#' @examples
#' N=20;n=12;k=5
#' plotHyper(N,n,k,type="d")
#' plotHyper(N,n,k,type="p",col="pink")
#' plotHyper(N,n,k)

plotHyper<-function(N,n,k,type="b",col="grey"){
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
  old <- options() ; on.exit(options(old))

  options(scipen=999)
  if(!is.numeric(N)|!is.vector(N)|any(!is.finite(N))) stop("The parameter 'N' must be a positive integer")
  if(!is.numeric(n)|!is.vector(n)|any(!is.finite(n))) stop("The parameter 'n' must be a positive integer")
  if(!is.numeric(k)|!is.vector(k)|any(!is.finite(k))) stop("The parameter 'k' must be a positive integer")
  if(length(N)!=1) stop("The parameter 'N' must be a positive integer")
  if(length(n)!=1) stop("The parameter 'n' must be a positive integer")
  if(length(k)!=1) stop("The parameter 'k' must be a positive integer")
  if((N<=0)|N!=round(N)) stop("The parameter 'N' must be a positive integer")
  if((n<=0)|n!=round(n)) stop("The parameter 'n' must be a positive integer")
  if((k<=0)|k!=round(k)) stop("The parameter 'k' must be a positive integer")
  if(n>N) stop("The parameter 'n' must be an integer smaller than N")
  if(k>N) stop("The parameter 'k' must be an integer smaller than N")
  if(!type%in%c("b","d","p")) stop("The argument 'type' must be 'b', 'd' or 'p'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  x=max(0,n+k-N):min(k,N)
  px=dhyper(x, m=k, n=N-k, k=n)
  Fx=phyper(x,m=k, n=N-k, k=n)
  if(is.null(type)){type="b"}

  if(type=="b"){
    par(mfrow=c(1,2))
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( H(N,n,k) = x)",main="Probability Mass")

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
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( H(N,n,k) = x)",main="Probability Mass")
  }
  res=round(as.matrix(cbind(x,px,Fx)),5)
  dimnames(res) <-list(rep("", dim(res)[1]), c("x","P(H(N,n,k)=x)","P(H(N,n,k)<=x)"))

  cat("Probability mass and distribution function associated with a H(N,n,k)\n \n")
  return(res)
}
