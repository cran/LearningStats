#' Plot a Histogram
#'
#' The function \code{Histogram} plots a histogram of a given sample.
#'
#' @param x a numeric vector containing the sample provided to compute the histogram.
#' @param freq a single logical value; if TRUE, the histogram graphic uses absolute frequencies; if FALSE (default), a histogram of area 1 (density) is plotted.
#' @param col.fill a single colour to be used to fill the bars; default to grey.
#' @param main main title, by default "Histogram" or "Histogram of area 1" depending on the value of the argument freq, TRUE or FALSE respectively.
#' @param xlab x-axis label; by default empty.
#' @param ylab y-axis label; by default "Frequency" or "Density" depending on the value of the argument freq, TRUE or FALSE respectively.
#'
#' @details The procedure to construct the histogram is detailed below:
#'
#'    - number of intervals: the closest integer to sqrt(n);
#'
#'    - amplitude of each interval: the range of the sample divided by the number of intervals, i.e., the breaks are equidistant and rounded to two decimals;
#'
#'    - height of each bar: by default (freq=FALSE) the plotted histogram is a density (area 1); if freq=TRUE, then the values of the bars are the absolute frequencies.
#'
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
#' Histogram(x)
#' Histogram(x,freq=TRUE)
#' Histogram(x,freq=TRUE,col="pink")
#'
#' @export
Histogram<-function(x, freq=FALSE, col.fill="grey", main="autom", xlab="", ylab="autoy"){
  if(!is.null(dim(x))){stop("'x' must be a numeric vector")}
  if (!is.numeric(x)|(sum(!is.finite(x))!=0)){stop("'x' must be a numeric vector")}
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
  if(!length(x)>1){stop("'x' must be a sample of size bigger than one")}
  if(length(freq)!=1){stop("'freq' must be a single logical value")}
  if (!is.logical(freq)){stop("'freq' must be a single logical value")}
  if(length(col.fill)!=1) stop("The argument 'col.fill' must be a single colour")
  if(col.fill%in%c(NA,NaN,Inf,-Inf)) stop("The argument 'col.fill' must be a single colour")

  old <- options() ; on.exit(options(old))

  n=length(x)
  ran=max(x)-min(x)
  n_int=round(sqrt(n),0)
  amp=ran/n_int; amp=ceiling(amp*100)/100
  breaks=min(x) + 0:n_int*amp;breaks=round(breaks,2);
  if(min(x)<breaks[1]){breaks[1]=breaks[1]-0.01}
  if(max(x)>=breaks[length(breaks)]){breaks[length(breaks)]=breaks[length(breaks)]+0.01}
  ni=table(cut(x,breaks, right=FALSE))
  fi=ni/n
  Ni=cumsum(ni)
  Fi=cumsum(fi)
  tab=round(cbind(ni,fi,Ni,Fi),2)
  di=fi/amp


  if(freq==TRUE){ #with absolute frequencies
    if(main=="autom"){main="Histogram"}
    if(ylab=="autoy"){ylab="Frequency"}
    suppressWarnings(hist(x,breaks=breaks,ylab=ylab,xlab=xlab, main=main, col=col.fill, axes=FALSE, freq=freq))
    rug(x)
    axis(1,at=round(breaks,2))
    axis(2,at=0:max(ni))
  }else if(freq==FALSE){ #with area 1
    if(main=="autom"){main="Histogram of area 1"}
    if(ylab=="autoy"){ylab="Density"}
    hist(x,breaks=breaks,ylab=ylab,xlab=xlab, main=main, col=col.fill, axes=FALSE, freq=freq)
    rug(x)
    axis(1,at=round(breaks,2))
    axis(2,at=seq(0,max(di)+0.1,by=0.1))
  }

  cat("\n          Frequency table\n \n")
  print(tab)
  invisible(list(ni=ni, fi=fi, Ni=Ni, Fi=Fi, tab=tab))}
