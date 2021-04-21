#' Boxplot Representation
#'
#' The function \code{BoxPlot} displays a boxplot representation of a given sample.

#' @param x a numeric vector containing the sample to plot the boxplot representation.
#' @param col a single colour to fill the boxplot representation; default to "white".
#' @param main a main title for the boxplot; default to "Boxplot representation".
#' @param ylab y-axis label; default to empty.
#' @param legend logical value; if TRUE (default), details about boxplot representation are given.
#'
#' @export
#' @import graphics
#'
#' @details The quantiles needed to obtain this representation are computed using the function \code{sample.quantile}.
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' x=c(5,-5,rnorm(40))
#' BoxPlot(x,col="pink")

BoxPlot<-function(x,col="white",main="Boxplot representacion",ylab="",legend=TRUE){
  if (!is.numeric(x)|!is.vector(x)){stop("The sample 'x' must be a numeric vector")}
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; print("Missing values have been removed from 'x'")}
  if (any(!is.finite(x))){stop(" The sample 'x' must be a numeric vector")}
  if(!length(x)>1){stop("'x' must be a sample of size bigger than one")}
  if(length(legend)!=1){stop("'legend' must be a single logical value")}
  if(!is.logical(legend)|is.na(legend)){stop("'legend' must be a single logical value")}
  if(length(col)!=1) stop("The argument 'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col' must be a single colour")


  Q=sample.quantile(x,tau=c(0.25,0.5,0.75))
  LS=min(max(x),Q[3]+1.5*(Q[3]-Q[1]))
  LI=max(min(x),Q[1]-1.5*(Q[3]-Q[1]))

  if(legend=="TRUE"){max=4.5}else{max=3.5}
  plot(x,type="n",xlim=c(0.5,max),ylim=c(min(x),max(x)),ylab=ylab,xaxt="n",xlab="",main=main)
  polygon(c(1,3,3,1),c(Q[1],Q[1],Q[3],Q[3]),col = col)
  segments(1,Q[2],3,Q[2],lwd=4)
  segments(1,Q[c(1,3)],3,Q[c(1,3)],lwd=2)
  segments(c(1,3),Q[1],c(1,3),Q[3],lwd=2)
  segments(2,Q[c(1,3)],2,c(LI,LS),lwd=2)
  segments(1.5,c(LI,LS),2.5,c(LI,LS),lwd=2)

  if(legend=="TRUE"){
    segments(3,Q[2],3.5,Q[2],lty=3);text(3.8,Q[2],bquote(~ Q[2] == .(round(Q[2],2))),cex=1.3)
    segments(3,Q[1],3.5,Q[1],lty=3);text(3.8,Q[1],bquote(~ Q[1] == .(round(Q[1],2))),cex=1.3)
    segments(3,Q[3],3.5,Q[3],lty=3);text(3.8,Q[3],bquote(~ Q[3] == .(round(Q[3],2))),cex=1.3)
    segments(2.5,LS,3.5,LS,lty=3);text(3.8,LS,bquote(~ LS == .(round(LS,2))),cex=1.3)
    segments(2.5,LI,3.5,LI,lty=3);text(3.8,LI,bquote(~ LI == .(round(LI,2))),cex=1.3)
  }

  if(sum((x<LI)| (x>LS))!=0){
    outlier=x[(x<LI)| (x>LS)]
    points(rep(2,length(outlier)),outlier,pch=19)
    if(legend=="TRUE"){segments(2,outlier[1],3.5,outlier[1],lty=3);text(3.8,outlier[1],"Outliers",cex=1.3)}
  }

}

