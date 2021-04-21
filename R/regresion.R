#' Representation of a Linear Regression Model
#'
#' @param x a numeric vector that contains the values of the explanatory variable.
#' @param y a numeric vector that contains the values of the response variable.
#' @param main a main title for the plot; default to "Linear Regression Model".
#' @param xlab x-axis label; default to "Explanatory variable (X)".
#' @param ylab y-axis label; default to "Response variable (Y)".
#' @param col.points a single colour associated with the sample points; default to "black".
#' @param col.line a single colour associated with the fitted linear regression model; default to "red".
#' @param pch an integer specifying a symbol or a single character to be used as the default in plotting points; default to 19.
#' @param lwd line width for the estimated model, a positive number; default to 2.
#' @param legend logical value; if TRUE (default), a legend with details about fitted model is included.
#'
#' @export
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' x=rnorm(100)
#' error=rnorm(100)
#' y=1+5*x+error
#' plotReg(x,y)


plotReg<-function(x,y,main="Linear Regression Model",xlab="Explanatory variable (X)",ylab="Response variable (Y)",col.points="black",col.line="red",pch=19,lwd=2,legend=TRUE){
  if(!is.numeric(x)|!is.vector(x)) stop("The explanatory variable must be univariate")
  if(!is.numeric(y)|!is.vector(y)) stop("The response variable must be univariate")
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; print("Missing values have been removed from 'x'")}
  if (any(!is.finite(x))){stop(" The sample 'x' must be a numeric vector")}
  if(sum(is.na(y))!=0){y=y[-which(is.na(y))]; print("Missing values have been removed from 'y'")}
  if (any(!is.finite(y))){stop(" The sample 'y' must be a numeric vector")}
  if(length(y)!=length(x)) stop("The length of 'x' and 'y' must be the same")
  if(length(y)==1) stop("The length of 'x' and 'y' must be bigger than 1")
  if(length(col.points)!=1) stop("The argument 'col.points' must be a single colour")
  if(col.points%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.points' must be a single colour")
  if(length(col.line)!=1) stop("The argument 'col.line' must be a single colour")
  if(col.line%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.line' must be a single colour")
  if(length(pch)!=1) stop("The argument 'pch' must be an integer specifying a symbol or a single character")
  if(is.character(pch)&nchar(pch)!=1) stop("The argument 'pch' must be an integer specifying a symbol or a single character")
  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")
  if(length(legend)!=1){stop("'legend' must be a single logical value")}
  if(!is.logical(legend)|is.na(legend)){stop("'legend' must be a logical value")}

  plot(x,y,pch=pch,col=col.points,xlab=xlab,ylab=ylab,main=main)
  mod=lm(y~x);abline(mod,col=col.line,lwd=lwd)

  if(legend=="TRUE"){
    if(coef(mod)[2]>0){
      legend("topleft",c(as.expression(bquote(widehat(Y) == .(round(coef(mod)[1],2))+.(round(coef(mod)[2],2)) * X)),
             as.expression(bquote(R^2 == .(round(summary(mod)$r.squared,2))))), bty="n",cex=1.2)
    }else{
      legend("topright",c(as.expression(bquote(widehat(Y) == .(round(coef(mod)[1],2)) - .(abs(round(coef(mod)[2],2))) * X)),
             as.expression(bquote(R^2 == .(round(summary(mod)$r.squared,2))))), bty="n",cex=1.2)

    }
  }

}




