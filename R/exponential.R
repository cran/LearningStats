#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a Exponential Distribution
#'
#' \code{plotExp} represents density, distribution and/or quantile functions associated with a Exponential
#'  distribution with certain parameter \code{lambda}.
#'
#' @param lambda the parameter of the Exponential distribution (1/mean).
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
#' lambda=0.5
#' plotExp(lambda)
#' plotExp(lambda,col="red")
#' plotExp(lambda,type="q")
#' plotExp(lambda,type="dis")
#' plotExp(lambda,type="den")


plotExp<-function(lambda,type="b",col="black"){
	if(!is.numeric(lambda) | !is.vector(lambda)|any(!is.finite(lambda))) stop("The parameter 'lambda' must be a single positive number")
  if(length(lambda)!=1 ) stop("The parameter 'lambda' must be a single positive number")
  if(lambda<=0) stop("The parameter 'lambda' must be a single positive number")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))

	x=seq(0,qexp(0.999,rate=lambda),by=0.001)
	fx=dexp(x,rate=lambda)
	Fx=pexp(x,rate=lambda)
	Finvx=qexp(seq(0,1,by=0.01),rate=lambda)

	if(type=="b"){
		par(mfrow=c(1,3))
		plot(x,fx,type="l",main="Density Function",xlim=c(-max(x)/10,max(x)),ylab="f(x)",lwd=2,col=col)
		segments(-max(x)/10,0,0,0,col=col,lwd=2)
		segments(0,0,0,dexp(0,rate=lambda),col=col,lwd=2,lty=2)

		plot(x,Fx,type="l",main="Distribution Function",xlim=c(-max(x)/10,max(x)),ylab="F(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")
		segments(-max(x)/10,0,0,0,col=col,lwd=2)

		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
		plot(x,Fx,type="l",main="Distribution Function",xlim=c(-max(x)/10,max(x)),ylab="F(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")
		segments(-max(x)/10,0,0,0,col=col,lwd=2)
	}else if(type=="den"){
		plot(x,fx,type="l",main="Density Function",xlim=c(-max(x)/10,max(x)),ylab="f(x)",lwd=2,col=col)
		segments(-max(x)/10,0,0,0,col=col,lwd=2)
		segments(0,0,0,dexp(0,rate=lambda),col=col,lwd=2,lty=2)
	}else if(type=="q"){
		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}
