
#' Illustration of the Normal approximation to Binomial
#'
#' When certain conditions are met (see Details), the Binomial distribution can be approximated by the Normal one. The function \code{AproxBinomNorm} illustrates this fact by plotting the mass diagram corresponding with the discrete distribution (parameters are given by the user) on which the associated Normal density function is also displayed.
#'
#' @param n number of independent Bernouilli trials.
#' @param p probability of success associated with the Bernouilli trial.
#' @param legend logical argument indicating whether to display the legend on the plot or not, default to TRUE.
#' @param xlab x-axis label; default to empty.
#' @param ylab y-axis label; default to "Probability."
#' @param main title; default to "Normal approximation to Binomial".
#' @param col.fill colour to fill-in the bars; default to grey.
#' @param col.line colour to draw the line of the Normal density; default to red.
#' @param lwd line width for the Normal density, a positive number; default to 2.
#'
#' @details The approximation is accurate only if one of these three conditions is met:
#'
#' - p in (0.1, 0.9) and n>=30,
#'
#' - p in [0,0.1] and np>5,
#'
#' - p in [0.9,1] and n(1-p)>5.
#'
#' @return This function is called for the side effect of drawing the plot.
#'
#' @examples
#' n=45; p=0.4
#' AproxBinomNorm(n,p)
#' AproxBinomNorm(n,p,col.fill="blue",col.line="orange")
#' AproxBinomNorm(n,p,legend=FALSE)
#'
#' @export
AproxBinomNorm<-function(n,p,legend=TRUE,xlab="", ylab="Probability", main="Normal approximation to Binomial", col.fill="grey", col.line="red",lwd=2){
  if(length(n)!=1){stop("'n' must be a single positive integer")}
  if (any(!is.finite(n))){stop("'n' must be a single positive integer")}
  if(!is.wholenumber(n)){stop("'n' must be a positive integer")}
  if(n<=0){stop("'n' must be a single positive integer")}

  if(length(p)!=1){stop("'p' must be a single number in (0,1)")}
  if (any(!is.finite(n))){stop("'p' must be a single number in (0,1)")}
  if(p<=0 | p>=1){stop("'p' must be a single number in (0,1)")}


  if(is.null(legend)){legend=TRUE}
  if(!is.null(legend)){if(!is.logical(legend)){stop("'legend' must be a single logical value")}}

  if(length(col.line)!=1) stop("The argument 'col.line' must be a single colour")
  if(col.line%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.line' must be a single colour")
  if(length(col.fill)!=1) stop("The argument 'col.fill' must be a single colour")
  if(col.fill%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.fill' must be a single colour")

  if(p>0.1 & p<0.9){if(n<30){warning("The aproximation might not be accurate because p is in (0.1,0.9) but n < 30")}}
  if(p<=0.1){if(n*p<=5){warning("The aproximation might not be accurate because p < 0.1 but np <= 5")}}
  if(p>=0.9){if(n*(1-p)<=5){warning("The aproximation might not be accurate because p > 0.9 but n(1-p) <= 5")}}

  if(is.null(xlab)){xlab=""}
  if(is.null(ylab)){ylab="Probability"}
  if(is.null(main)){main="Normal approximation to Binomial"}
  if(is.null(col.fill)){col.fill="grey"}
  if(is.null(col.line)){col.line="red"}
  if(is.null(lwd)){lwd=2}
  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")


  mu=n*p
  sigma=sqrt(n*p*(1-p))

  xi=0:n
  pi=dbinom(xi,n,p)




  #barplot(as.table(pi),names.arg=xi,ylim=c(0,max(pi)+0.03),xlab="",ylab="Probability",main="Normal approximation to binomial")
   plot(0:(n+1),type="n", xlim=c(-1,n+1),ylim=c(0,max(pi)+0.05),xlab=xlab,ylab=ylab,axes=F,main=main)
   axis(1,at=c(-1,xi,max(xi)+1))
   axis(2,at=round(seq(0,(min(1,max(pi)+0.02)),len=5),2))
   segments(x0=xi-0.3,y0=0,x1=xi-0.3,y1=pi)
   segments(x0=xi+0.3,y0=0,x1=xi+0.3,y1=pi)
   segments(x0=xi-0.3, y0=pi, x1=xi+0.3, y1=pi)
   points(xi,pi,pch=16)
   for(i in 1:(n+1)){polygon(x=c(i-1-0.3,seq(i-1-0.3,i-1+0.3,len=100),i-1+0.3), y=c(0,rep(pi[i],100),0), col=col.fill)}

  lines(seq(-1,n,len=1000),dnorm(seq(-1,n,len=1000),mean=mu,sd=sigma),col=col.line,lwd=lwd)
  if(legend==TRUE){legend("topright",legend = c(paste("Bi ( ",n," , ",round(p,2)," )",sep=""),paste("N ( ",round(mu,2)," , ",round(sigma,2)," )",sep="")), col=c(col.fill,col.line), lwd=c(NA,lwd), pch=c(15,NA), bty="n")}

  # cat(paste("The distributions plotted are Bi(",n,",",round(p,2),") and N(",round(mu,2),",",round(sigma,2),").", sep=""))
  }


is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
