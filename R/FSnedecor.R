#' Density Function, Distribution Function and/or
#' Quantile Function Representations associated with a F-Snedecor Distribution
#'
#' \code{plotBeta} represents density, distribution and/or quantile functions associated with a F-Snedecor
#'  distribution with certain \code{df1} and \code{df2} degrees of freedom.

#'
#' @param df1,df2 the degrees of freedom of the F-Snedecor distribution.
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
#' df1=10;df2=15
#' plotFS(df1,df2)
#' plotFS(df1,df2,col="red")
#' plotFS(df1,df2,type="q")
#' plotFS(df1,df2,type="dis")
#' plotFS(df1,df2,type="den")

plotFS<-function(df1,df2,type="b",col="black"){
  if(!is.numeric(df1)|!is.vector(df1)|any(!is.finite(df1))) stop("The parameter 'df1' (degrees of freedom) must be a single positive integer")
  if(length(df1)!=1) stop("The parameter 'df1' (degrees of freedom) must be a single positive integer")
  if(df1<=0 | df1!=as.integer(df1)) stop("The parameter 'df1' (degrees of freedom) must be a single positive integer")
  if(!is.numeric(df2)|!is.vector(df2)|any(!is.finite(df2))) stop("The parameter 'df2' (degrees of freedom) must be a single positive integer")
  if(length(df2)!=1) stop("The parameter 'df2' (degrees of freedom) must be a single positive integer")
  if(df2<=0 | df2!=as.integer(df2)) stop("The parameter 'df2' (degrees of freedom) must be a single positive integer")
  if(!type%in%c("b","dis","den","q")) stop("The argument 'type' must be 'b', 'dis', 'den' or 'q'")
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))

  quant=qf(c(0.95,0.999),df1=df1,df2=df2)
  if(abs(diff(quant))<10){max=quant[2]}else{max=quant[1]}
	x=seq(0,max,by=0.005)
	dom=max(x)-min(x)
	fx=df(x,df1=df1,df2=df2)
	Fx=pf(x,df1=df1,df2=df2)
	Finvx=qf(seq(0,1,by=0.01),df1=df1,df2=df2)

	if(type=="b"){
		par(mfrow=c(1,3))
		plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col,xlim=c(-dom/15,max(x)))
        segments(-dom/10,0,0,0,lwd=2,col=col)

		plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col,ylim=c(0,1),xlim=c(-dom/10,max(x)))
		abline(h=c(0,1),lty=2,col="gray")
		segments(-dom/10,0,0,0,lwd=2,col=col)

		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}else if(type=="dis"){
	  plot(x,Fx,type="l",main="Distribution Function",ylab="F(x)",lwd=2,col=col,ylim=c(0,1),xlim=c(-dom/10,max(x)))
	  abline(h=c(0,1),lty=2,col="gray")
	  segments(-dom/10,0,0,0,lwd=2,col=col)
	}else if(type=="den"){
	  plot(x,fx,type="l",main="Density Function",ylab="f(x)",lwd=2,col=col,xlim=c(-dom/10,max(x)))
	  segments(-dom/10,0,0,0,lwd=2,col=col)
	}else if(type=="q"){
		plot(seq(0,1,by=0.01),Finvx,type="l",xlab=expression(tau),ylab="", main="Quantile Function",lwd=2,col=col)
		title(ylab=expression(paste("F"^"-1",(tau),sep="")), line=2.5, cex.lab=1)
	}

}
