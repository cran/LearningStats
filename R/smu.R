#' Standard Deviation Estimator when the Population Mean is Known
#'
#' \code{Smu} computes a estimation of the standard deviation, given a sample \code{x} with known mean (denoted by \code{mu}).

#' @param x a numeric vector containing the sample.
#' @param mu the population mean.
#'
#'@details Given \eqn{\{x_1,\ldots,x_n\}} a sample of a random variable, the standard deviation estimator
#' when the population mean (denoted by \eqn{\mu}) is known can be computed as \eqn{S_\mu=\sqrt{\frac{1}{n}\sum_{i=1}^n (x_i-\mu)^2}}.
#'
#' @export
#'
#' @return A single numerical value corresponding with the standard deviation estimation when the population mean is known.
#'
#' @examples
#' x=rnorm(20)
#' Smu(x,mu=0)

Smu<-function(x,mu){
	if (!is.numeric(mu)|any(!is.finite(mu))){stop("The mean 'mu' must be a single number")}
    if (!is.vector(mu) | length(mu)!=1){stop("The mean 'mu' must be a single number")}
	if (!is.numeric(x)|!is.vector(x)){stop(" The sample 'x' must be a numeric vector")}
	if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
	if (any(!is.finite(x))){stop(" The sample 'x' must be a numeric vector")}
	if(!length(x)>1){stop("'x' must be a sample of size bigger than one")}

	sqrt(sum((x-mu)^2)/length(x))
}
