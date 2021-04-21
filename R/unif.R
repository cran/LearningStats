#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a Uniform Distribution
#'
#' \code{plotUnif} represents density, distribution and/or quantile functions associated with a Uniform
#'  distribution with \code{min} and \code{max} the lower and upper limits, respectively.
#'
#' @param min minimum value of the Uniform distribution.
#' @param max maximum value of the Uniform distribution.
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
#' min=0 ; max=1
#' plotUnif(min,max)
#' plotUnif(min,max,col="red")
#' plotUnif(min,max,type="q")
#' plotUnif(min,max,type="dis")
#' plotUnif(min,max,type="den")



plotUnif<-function(min,max,type="b",col="black"){
  if (!is.numeric(min)|!is.vector(min)|any(!is.finite(min))){stop("The minimun of the uniform distribution, 'min', must be a single number")}
  if (length(min)!=1){stop("The minimun of the uniform distribution, 'min', must be a single number")}
  if (!is.numeric(max)|!is.vector(max)|any(!is.finite(max))){stop("The maximun of the uniform distribution, 'max', must be a single number")}
  if (length(max)!=1){stop("The maximun of the uniform distribution, 'max', must be a single number")}
  if((max-min)<=0) stop("The parameters min and max represent the minimum and maximum values of the uniform distribution, respectively")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

	dom=(max-min)/10
	x=seq(min-dom,max+dom,by=0.001)
	fx=dunif(x,min=min,max=max)
	Fx=punif(x,min=min,max=max)
	Finvx=qunif(seq(0,1,by=0.01),min=min,max=max)

	if(type=="b"){
		par(mfrow=c(1,3))
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(min,0,min,1/(max-min),lty=2,col="white",lwd=3)
		segments(max,0,max,1/(max-min),lty=2,col="white",lwd=3)

		plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")

		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
		plot(x,Fx,type="l",main="Distribution Function",ylab="f(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")
	}else if(type=="den"){
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(min,0,min,1/(max-min),lty=2,col="white",lwd=3)
		segments(max,0,max,1/(max-min),lty=2,col="white",lwd=3)
	}else if(type=="q"){
		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}
