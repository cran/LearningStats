
#' Illustration of the Normal approximation to Poisson
#'
#' When certain conditions are met (see Details), the Poisson distribution can be approximated by the Normal one. The function \code{AproxPoisNorm} illustrates this fact by plotting the mass diagram corresponding with the discrete distribution (parameter is given by the user) on which the associated Normal density function is also displayed.
#'
#' @param lambda mean of the Poisson distribution.
#' @param legend logical argument indicating whether to display the legend on the plot or not, default to TRUE.
#' @param xlab x-axis label; default to empty.
#' @param ylab y-axis label; default to "Probability".
#' @param main title; default to "Normal approximation to Poisson".
#' @param col.fill colour to fill the bars; default to grey.
#' @param col.line colour to draw the line of the Normal density; default to red.
#' @param lwd line width for the Normal density, a positive number; default to 2.
#'
#' @details The approximation is accurate only if lambda>=10.
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' lambda=15
#' AproxPoisNorm(lambda)
#' AproxPoisNorm(lambda,col.fill="blue",col.line="orange")
#' AproxPoisNorm(lambda,legend=FALSE)
#'
#' @export
AproxPoisNorm<-function(lambda,legend=TRUE, xlab="", ylab="Probability", main="Normal approximation to Poisson", col.fill="grey", col.line="red", lwd=2){
  if(length(lambda)!=1){stop("'lambda' must be a single positive number")}
  if ((any(!is.finite(lambda)))){stop("'lambda' must be a single positive number")}
  if(lambda<=0){stop("'lambda' must be a single positive number")}

  if(!lambda>10){warning("The aproximation might not be accurate because 'lambda'<10")}

  if(is.null(legend)){legend=TRUE}
  if(!is.null(legend)){if(!is.logical(legend)){stop("'legend' must be a single logical value")}}

  if(length(col.line)!=1) stop("The argument 'col.line' must be a single colour")
  if(col.line%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.line' must be a single colour")
  if(length(col.fill)!=1) stop("The argument 'col.fill' must be a single colour")
  if(col.fill%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.fill' must be a single colour")

  if(is.null(xlab)){xlab=""}
  if(is.null(ylab)){ylab="Probabiility"}
  if(is.null(main)){main="Normal approximation to Poisson"}
  if(is.null(col.fill)){col.fill="grey"}
  if(is.null(col.line)){col.line="red"}
  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a single positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a single positive integer")


  mu=lambda
  sigma=sqrt(lambda)

  #n=ceiling(2*lambda)
  xi=0:(qpois(0.999,lambda)+2)
  pi=dpois(xi,lambda=lambda)
  #xi=0:n
  #pi=dpois(xi,lambda=lambda)


  plot(xi,type="n", xlim=c(0,max(xi)+1),ylim=c(0,max(pi)+0.05),xlab=xlab,ylab=ylab,axes=F, main=main)
  axis(1,at=xi)
  axis(2,at=round(seq(0,(max(pi)+0.02),len=5),2))
  segments(x0=xi-0.3,y0=0,x1=xi-0.3,y1=pi)
  segments(x0=xi+0.3,y0=0,x1=xi+0.3,y1=pi)
  segments(x0=xi-0.3, y0=pi, x1=xi+0.3, y1=pi)
  points(xi,pi,pch=16)
  for(i in 1:(length(xi)+1)){polygon(x=c(i-1-0.3,seq(i-1-0.3,i-1+0.3,len=100),i-1+0.3), y=c(0,rep(pi[i],100),0), col=col.fill)}

  lines(seq(0,max(xi),len=1000),dnorm(seq(0,max(xi),len=1000),mean=mu,sd=sigma),col=col.line,lwd=lwd)
  if(legend==TRUE){legend("topright",legend = c(paste("Pois ( ",round(lambda,2)," )",sep=""),paste("N ( ",round(mu,2)," , ",round(sigma,2)," )",sep="")), col=c(col.fill,col.line),lwd=c(NA,lwd), pch=c(15,NA), bty="n")}


  #cat(paste("The distributions plotted are Pois(",round(lambda,2),") and N(",round(mu,2),",",round(sigma,2),").", sep=""))
  }

