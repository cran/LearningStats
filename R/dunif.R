
#' Probability Mass and/or Distribution Function Representations associated with a Discrete Uniform Distribution
#'
#' \code{plotDUnif} represents the probability mass and/or the distribution function associated with a Discrete Uniform
#'  distribution with support \code{x}.
#'
#' @param x support of the discrete variable.
#' @param type 	a character string giving the type of desired plot. The following values are possible:
#' "b" (default) for probability mass function and distribution function representations together,
#' "d" for distribution function representation and "p" for probability mass function representation.
#' @param col a single colour associated with the probability mass function representation; default to "grey".
#'
#' @return A matrix containing the probability mass and the distribution function associated with each point
#' of the support (denoted by \code{x}) of a Discrete Uniform distribution.
#' @export
#'
#' @examples
#' x=1:5
#' plotDUnif(x,type="d")
#' plotDUnif(x,type="p",col="pink")
#' plotDUnif(x)

plotDUnif<-function(x,type="b",col="grey"){
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
  old <- options() ; on.exit(options(old))

  options(scipen=999)
  if(!is.numeric(x)|!is.vector(x)|any(!is.finite(x))) stop("The support of the discrete variable must be numeric vector")
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; print("Missing values have been removed from 'x'")}
  if(length(unique(x))!=length(x)) stop("The support of the discrete variable must not contain ties")
  if(!type%in%c("b","d","p")) stop("The argument 'type' must be 'b', 'd' or 'p'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")


  x=sort(x)
  n=length(x)
  px=rep(1/n,n)
  Fx=cumsum(px)
  if(n>30){warning("The support of the considered Discrete Uniform contains more than 30 points. Are you sure that you want to consider this kind of discrete distribution?")}

  if(type=="b"){
    par(mfrow=c(1,2))
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( DUnif(x1,...,xn) = x)",main="Probability Mass")

    plot(c(-1,x),c(0,Fx),type="s",xlim=c(min(x)-1,max(x)+1),lty=2,lwd=2,ylab="F(x)",xlab="x",main="Distribution Function",xaxt="n")
    points(x,Fx,pch=19,cex=1.2)
    segments(c(-1,x),c(0,Fx),c(0,x[2:n],max(x)+1),c(0,Fx),lwd=2)
    axis(1,at=x,labels=x,las=1)
    abline(h=c(0,1),lty=2,col="grey")
  }else if(type=="d"){
    plot(c(-1,x),c(0,Fx),type="s",lty=2,lwd=2,ylab="F(x)",xlab="x",main="Distribution Function",xaxt="n")
    points(x,Fx,pch=19,cex=1.2)
    segments(c(-1,x),c(0,Fx),c(0,x[2:n],max(x)+1),c(0,Fx),lwd=2)
    axis(1,at=x,labels=x,las=1)
    abline(h=c(0,1),lty=2,col="grey")
  }else if(type=="p"){
    barplot(as.table(px),names.arg=x,ylim=c(0,max(px)+0.03),col=col,xlab="x",ylab="P( DUnif(x1,...,xn) = x)",main="Probability Mass")
  }
  res=round(as.matrix(cbind(x,px,Fx)),5)
  dimnames(res) <-list(rep("", dim(res)[1]), c("x","P(DUnif(x1,...,xn)=x)","P(DUnif(x1,...,xn) <=x)"))

  cat("Probability mass and distribution function associated with a DUnif(x1,...,xn)\n \n")
  return(res)
}
