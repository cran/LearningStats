#' Plot a Cumulative Frequency Polygon
#'
#' The function \code{freq.pol} computes a cumulative frequency polygon of a given sample.
#'
#' @param x a numeric vector containing the sample to compute the cumulative polygon.
#' @param freq logical value; if TRUE, the cumulative polygon uses absolute frequencies; if FALSE, relative frequencies are used.
#' @param col colour to be used in the polygon line; default to black.
#' @param lwd a single numeric value corresponding with the width to be used in the polygon line; default to 2
#' @param main main title; by default  to "Polygon of cumulative absolute frequencies" or "Polygon of cumulative relative frequencies" depending on the value of the argument freq, TRUE or FALSE respectively.
#' @param xlab x-axis label; by default to empty .
#' @param ylab y-axis label; by default to "Cumulative absolute frequencies" or "Cumulative relative frequencies" depending on the value of the argument freq, TRUE or FALSE respectively.
#' @param bar logical value; if TRUE (default), bars are plotted underneath the polygon line.
#' @param fill logical value; if TRUE bars are filled with colour set in col.fill (if not given pink is chosen); FALSE (default) unless col.fill is given.
#' @param col.fill colour to be used to fill the bars; if not given and fill=TRUE, set to pink.
#'
#' @details The sample must be numeric and coming from a continuous variable.
#'
#' The procedure used to define the intervals for the frequency table and the bars (if plotted) is the same as used for the histogram performed in this package (see ?Histogram).
#'
#' @return A list containing the following components:
#' \item{ni}{a numeric vector containing the absolute frequencies.}
#' \item{fi}{a numeric vector containing the relative frequencies.}
#' \item{Ni}{a numeric vector containing the absolute cumulative frequencies.}
#' \item{Fi}{a numeric vector containing the relative cumulative frequencies.}
#' \item{tab}{the frequency table.}
#' Independently on the user saving those values, the function provides the frequency table on the console.
#'
#' @examples
#' x=rnorm(10)
#' freq.pol(x)
#'
#' freq.pol(x,freq=TRUE,fill=TRUE,col.fill="yellow")
#'
#' @export
freq.pol<-function(x, freq=FALSE, col="black", lwd=2,  main="autom", xlab="", ylab="autoy", bar=TRUE, fill=TRUE, col.fill="pink"){
  if(!is.null(dim(x))){stop("'x' must be a numeric vector")}
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
  if(!length(x)>1){stop("'x' must be a sample of size bigger than one")}
  if(any(!is.finite(x))){stop("'x' must be a numeric vector")}
  if(!is.numeric(lwd)|length(lwd)!=1) stop("'lwd' must be a single positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("'lwd' must be a single positive integer")
  if (length(freq)!=1){stop("'freq' must be a single logical value")}
  if (!is.logical(freq)){stop("'freq' must be a single logical value")}
  if(length(col)!=1) stop("'col' must be a single colour")
  if(col%in%c(NA,NaN,Inf,-Inf)) stop("'col' must be a single colour")
  if(length(col.fill)!=1) stop("'col.fill' must be a single colour")
  if(col.fill%in%c(NA,NaN,Inf,-Inf)) stop("'col.fill' must be a single colour")


  n=length(x)
  ran=max(x)-min(x)
  n_int=round(sqrt(n),0)
  amp=ran/n_int; amp=ceiling(amp*100)/100
  breaks=min(x) + 0:n_int*amp;breaks=round(breaks,2)
  c0=min(x)+amp/2
  centers=c0+0:(length(breaks)-2)*amp
  ni=table(cut(x,breaks, right=FALSE))
  fi=ni/n
  Ni=cumsum(ni)
  Fi=cumsum(fi)
  tab=round(cbind(ni,fi,Ni,Fi),4)

  if(freq==TRUE){ #with absolute frequencies
    if(main=="autom"){main="Polygon of cumulative absolute frequencies"}
    if(ylab=="autoy"){ylab="Cumulative absolute frequencies"}
    plot(centers, Ni, type="b",ylim=c(0,max(Ni)), xlim=c(min(x),max(x)), axes=FALSE, ylab=ylab, xlab=xlab, main=main, lwd=lwd)
    axis(1,at=round(sort(c(breaks,centers)),2))
    axis(2,at=0:(max(Ni)+1))
    if(bar==TRUE){
    segments(breaks,rep(0, length(breaks)),breaks,c(Ni,max(Ni)))
    segments(breaks[-length(breaks)],Ni,breaks[-1],Ni)
    if(fill==TRUE){
      if(is.null(col.fill)){col.fill="pink"}
      for(i in 1:(length(breaks)-1)){polygon(c(breaks[i],seq(breaks[i],breaks[i+1],len=100),breaks[i+1]),c(0,rep(as.numeric(Ni[i]),100),0),col=col.fill)}
      lines(centers, Ni, type="b",lwd=lwd)}
    }

  }else if(freq==FALSE){ #with relative frequencies
    if(main=="autom"){main="Polygon of cumulative relative frequencies"}
    if(ylab=="autoy"){ylab="Cumulative relative frequencies"}
    plot(centers, Fi, type="b",ylim=c(0,1), xlim=c(min(x),max(x)), axes=FALSE, ylab=ylab, xlab=xlab, main=main, lwd=lwd)
    axis(1,at=round(sort(c(breaks,centers)),2))
    axis(2,at=seq(0,1,by=0.1))
    if(bar==TRUE){
      segments(breaks,rep(0, length(breaks)),breaks,c(Fi,1))
      segments(breaks[-length(breaks)],Fi,breaks[-1],Fi)
      if(fill==TRUE){
        if(is.null(col.fill)){col.fill="pink"}
          for(i in 1:(length(breaks)-1)){polygon(c(breaks[i],seq(breaks[i],breaks[i+1],len=100),breaks[i+1]),c(0,rep(as.numeric(Fi[i]),100),0),col=col.fill)}
          lines(centers, Fi, type="b",lwd=lwd)}
      }
  }

  cat("\n          Frequency table\n \n")
  print(tab)
  invisible(list(ni=ni, fi=fi, Ni=Ni, Fi=Fi, tab=tab))}

