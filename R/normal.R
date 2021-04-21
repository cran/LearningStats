#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a Normal Distribution
#'
#' \code{plotNorm} represents density, distribution and/or quantile functions associated with a Normal
#'  distribution with certain parameters \code{mu} and \code{sigma}.
#'
#' @param mu the mean of the Normal distribution.
#' @param sigma the standard deviation of the Normal distribution.
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
#' mu=10; sigma=5
#' plotNorm(mu,sigma)
#' plotNorm(mu,sigma,col="red")
#' plotNorm(mu,sigma,type="q")
#' plotNorm(mu,sigma,type="dis")
#' plotNorm(mu,sigma,type="den")

plotNorm<-function(mu,sigma,type="b",col="black"){
	if(!is.numeric(mu)|!is.vector(mu)|any(!is.finite(mu))){stop("The parameter 'mu' (mean) must be a single number")}
  if(length(mu)!=1){stop("The paramter 'mu' (mean) must be a single number")}
	if(!is.numeric(sigma)|!is.vector(sigma) |any(!is.finite(sigma))) stop("The parameter 'sigma' (standard deviation) must be a single positive number")
  if(length(sigma)!=1) stop("The parameter 'sigma' (standard deviation) must be a single positive number")
  if(sigma<=0) stop("The parameter 'sigma' (standard deviation) must be a single positive number")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))

	x=seq(-3*sigma+mu,3*sigma+mu,by=0.01)
	fx=dnorm(x,mean=mu,sd=sigma)
	Fx=pnorm(x,mean=mu,sd=sigma)
	Finvx=qnorm(seq(0,1,by=0.01),mean=mu,sd=sigma)

	if(type=="b"){
		par(mfrow=c(1,3))
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(mu,-1,mu,dnorm(mu,mean=mu,sd=sigma),lty=2,col="gray")

		plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")

		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
		plot(x,Fx,type="l",main="Distribution Function",ylab="f(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")
	}else if(type=="den"){
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(mu,-1,mu,dnorm(mu,mean=mu,sd=sigma),lty=2,col="gray")
	}else if(type=="q"){
		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}
