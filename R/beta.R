#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a Beta Distribution
#'
#' \code{plotBeta} represents density, distribution and/or quantile functions associated with a Beta
#'  distribution with parameters \code{shape1} and \code{shape2}.
#'
#' @param shape1,shape2 parameters of the Beta distribution (mean equal to \code{shape1}/(\code{shape1}+\code{shape2})
#' @param type a character string giving the type of plot desired. The following values are possible:
#' "b" (default) for density function, distribution function and quantile function representations together,
#' "dis" for distribution function representation,
#' "den" for density function representation and "q" for quantile function representation.
#' @param col a single colour associated with the different representations; default to "black".
#'
#' @export
#' @import stats
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' shape1=1;shape2=1
#' plotBeta(shape1,shape2)
#' plotBeta(shape1,shape2,col="red")
#' plotBeta(shape1,shape2,type="q")
#' plotBeta(shape1,shape2,type="dis")
#' plotBeta(shape1,shape2,type="den")


plotBeta<-function(shape1,shape2,type="b",col="black"){
	if(!is.numeric(shape1) | !is.vector(shape1)|any(!is.finite(shape1))) stop("The parameter 'shape1' must be a single positive number")
  if(length(shape1)!=1) stop("The parameter 'shape1' must be a single positive number")
  if(shape1<=0) stop("The parameter 'shape1' must be a single positive number")
  if(!is.numeric(shape2) | !is.vector(shape2)|any(!is.finite(shape2))) stop("The parameter 'shape2' must be a single positive number")
  if(length(shape2)!=1) stop("The parameter 'shape2' must be a single positive number")
  if(shape2<=0) stop("The parameter 'shape2' must be a single positive number")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))

	x=seq(0,1,by=0.01)
	fx=dbeta(x,shape1=shape1, shape2=shape2)
	Fx=pbeta(x,shape1=shape1, shape2=shape2)
	Finvx=qbeta(seq(0,1,by=0.01),shape1=shape1, shape2=shape2)

	if(type=="b"){
	  par(mfrow=c(1,3))
	  plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col,xlim=c(-0.1,1.1))
    if(fx[x==0]!=0){segments(0,0,0,fx[x==0],col=col,lwd=2,lty=2)}
	  if(fx[x==1]!=0){segments(1,0,1,fx[x==1],col=col,lwd=2,lty=2)}
	  segments(-0.1,0,0,0,lwd=2,col=col);segments(1,0,1.1,0,lwd=2,col=col)

	  plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col,xlim=c(-0.1,1.1))
	  abline(h=c(0,1),lty=2,col="gray")
	  segments(-0.1,0,0,0,lwd=2,col=col);segments(1,1,1.1,1,lwd=2,col=col)

	  plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
	  title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
	  plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col,xlim=c(-0.1,1.1))
	  abline(h=c(0,1),lty=2,col="gray")
	  segments(-0.1,0,0,0,lwd=2,col=col);segments(1,1,1.1,1,lwd=2,col=col)
	}else if(type=="den"){
	  plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col,xlim=c(-0.1,1.1))
	  if(fx[x==0]!=0){segments(0,0,0,fx[x==0],col=col,lwd=2,lty=2)}
	  if(fx[x==1]!=0){segments(1,0,1,fx[x==1],col=col,lwd=2,lty=2)}
	  segments(-0.1,0,0,0,lwd=2,col=col);segments(1,0,1.1,0,lwd=2,col=col)
	}else if(type=="q"){
	  plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
	  title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}

