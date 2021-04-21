#' Sample Quantiles
#'
#' \code{sample.quantile} computes a estimation of different quantiles, given a sample {x}, using order statistics.

#' @param x a numeric vector containing the sample.
#' @param tau the quantile(s) of interest, that must be a number(s) in (0,1).
#'
#' @return A number or a numeric vector of \code{tau}-quantile(s).
#' @export
#'
#' @details A quantile \code{tau} determines the proportion of values in a distribution are above or below a certain limit. For instance, given
#' \code{tau} a number between 0 and 1, the \code{tau}-quantile splits the sample into tow parts with probabilities \code{tau} and
#' \code{(1-tau)}, respectively.
#'
#' One possible way to calculate the quantile \code{tau} would be to ordering the sample
#' and taking as the quantile the smallest data in the sample (first of the ordered sample)
#' whose cumulative relative frequency is greater than \code{tau}.
#' If there is a point in the sample with a cumulative relative frequency equal to \code{tau},
#' then the sample quantile will be calculated as the mean between that point and the next one of the ordered sample.
#'
#' @return A numerical value or vector corresponding with the requested sample quantiles.
#'
#' @examples
#' x=rnorm(20)
#' sample.quantile(x,tau=0.5)
#' sample.quantile(x,tau=c(0.25,0.5,0.75))

sample.quantile<-function(x,tau){
  if (!is.numeric(tau)|!is.vector(tau)|any(!is.finite(tau))){stop("The quantile order 'tau' must be a single number")}
  if(sum((tau>=1)|(tau<=0))!=0) stop("The parameter 'tau' must be a single number or a vector between 0 and 1")
  if (!is.numeric(x)|!is.vector(x)){stop("The sample 'x' must be a numeric vector")}
  if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
  if (any(!is.finite(x))){stop(" The sample 'x' must be a numeric vector")}
  if(!length(x)>1){stop("'x' must be a sample of size bigger than one")}

  x=sort(x)
  quant=numeric(length(tau))
  for(i in 1:length(tau)){
    if((tau[i]<=0) | (tau[i]>=1)) stop("The parameter tau must be a vector of numbers between zero and one")
    ord=tau[i]*length(x)
    if(ord==as.integer(ord)){
      quant[i]=(x[ord]+x[ord+1])/2
    }else{
      quant[i]=x[as.integer(ord)+1]
    }
  }
  return(quant)
}
