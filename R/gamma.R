#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a Gamma Distribution
#'
#' \code{plotGamma} represents density, distribution and/or quantile functions associated with a Gamma
#'  distribution with certain parameters \code{lambda} and  \code{shape}.
#'
#' @param lambda,shape parameters of the Gamma distribution (mean equal to \code{shape}/\code{lambda}).
#' @param type a character string giving the type of desired plot. The following values are possible:
#' "b" (default) for density function, distribution function and quantile function representations together,
#' "dis" for distribution function representation,
#' "den" for density function representation and "q" for quantile function representation.
#' @param col a single colour associated with the different representations; default to "black".
#'
#' @export
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' lambda=0.5;shape=4
#' plotGamma(lambda,shape)
#' plotGamma(lambda,shape,col="red")
#' plotGamma(lambda,shape,type="q")
#' plotGamma(lambda,shape,type="dis")
#' plotGamma(lambda,shape,type="den")


plotGamma<-function(lambda,shape,type="b",col="black"){
	if(!is.numeric(shape)|!is.vector(shape)|any(!is.finite(shape))){stop("The parameter 'shape' must be a single number")}
  if(length(shape)!=1){stop("The parameter 'shape' must be a single number")}
  if(!is.numeric(lambda) | !is.vector(lambda)|any(!is.finite(lambda))) stop("The parameter 'lambda' must be a positive number")
  if(length(lambda)!=1 ) stop("The parameter 'lambda' must be a positive number")
  if(lambda<=0) stop("The parameter 'lambda' must be a positive number")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))

	x=seq(0,qgamma(0.999,shape=shape,rate=lambda),by=0.01)
	fx=dgamma(x,shape=shape,rate=lambda)
	Fx=pgamma(x,shape=shape,rate=lambda)
	Finvx=qgamma(seq(0,1,by=0.01),shape=shape,rate=lambda)

	if(type=="b"){
		par(mfrow=c(1,3))
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(-max(x)/10,0,0,0,col=col,lwd=2)
    if(fx[1]!=0){segments(0,0,0,fx[1],col=col,lwd=2,lty=2)}

		plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col)
		segments(-max(x)/10,0,0,0,col=col,lwd=2)
		abline(h=c(0,1),lty=2,col="gray")

		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
		plot(x,Fx,type="l",main="Distribution Function",ylab="f(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")
		segments(-max(x)/10,0,0,0,col=col,lwd=2)
	}else if(type=="den"){
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
	  segments(-max(x)/10,0,0,0,col=col,lwd=2)
	  if(fx[1]!=0){segments(0,0,0,fx[1],col=col,lwd=2,lty=2)}
	}else if(type=="q"){
		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}

