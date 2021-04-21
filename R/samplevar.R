#' Sample Variance
#'
#' \code{sample.var} computes the sample variance of a sample \code{x}.

#' @param x a numeric vector containing the sample.
#'
#' @details Given \eqn{\{x_1,\ldots,x_n\}} a sample of a random variable, the sample variance
#' can be computed as \eqn{S^2=\frac{1}{n}\sum_{i=1}^n (x_i-\bar{x})^2}.
#'
#' @export
#'
#' @return A single numerical value corresponding with the sample variance.
#'
#'
#' @examples
#' x=rnorm(20)
#' sample.var(x)

sample.var<-function(x){
	if (!is.numeric(x)|!is.vector(x)){stop("The sample 'x' must be a numeric vector")}
	if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
	if (any(!is.finite(x))){stop(" The sample 'x' must be a numeric vector")}
	if(!length(x)>1){stop("'x' must be a sample of size bigger than 1")}

	sum((x-mean(x))^2)/length(x)
}
