#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a T-Student Distribution
#'
#' \code{plotTS} represents density, distribution and/or quantile functions associated with a T-Student
#'  distribution with \code{df} degrees of freedom.
#'
#' @param df the degrees of freedom of the T-Student distribution.
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
#' df=10
#' plotTS(df)
#' plotTS(df,col="red")
#' plotTS(df,type="q")
#' plotTS(df,type="dis")
#' plotTS(df,type="den")



plotTS<-function(df,type="b",col="black"){
  if(!is.numeric(df)|!is.vector(df)|any(!is.finite(df))) stop("The parameter df (degrees of freedom) must be a single positive integer")
  if(length(df)!=1) stop("The parameter df (degrees of freedom) must be a single positive integer")
  if(df<=0 | df!=as.integer(df)) stop("The parameter df (degrees of freedom) must be a single positive integer")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

	x=seq(qt(0.001,df=df),qt(0.999,df=df),by=0.01)
	fx=dt(x,df=df)
	Fx=pt(x,df=df)
	Finvx=qt(seq(0,1,by=0.01),df=df)

	if(type=="b"){
		par(mfrow=c(1,3))
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(0,-1,0,dt(0,df=df),lty=2,col="gray")

		plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")

		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
		plot(x,Fx,type="l",main="Distribution Function",ylab="f(x)",lwd=2,col=col)
		abline(h=c(0,1),lty=2,col="gray")
	}else if(type=="den"){
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col)
		segments(0,-1,0,dt(0,df=df),lty=2,col="gray")
	}else if(type=="q"){
		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}
