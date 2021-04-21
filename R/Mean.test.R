
#' One Sample Mean Test of a Normal Population
#'
#' \code{Mean.test} allows to compute hypothesis tests for a Normal population mean in both scenarios:
#' known and unknown population variance.
#'
#' @param x a numeric vector of data values or, if single number, estimated mean.
#' @param mu0 a single number corresponding with the mean to test.
#' @param sigma population standard deviation. Defaults to NULL, if specified, a t-test with
#' known variance will be performed.
#' @param sc cuasi-standard deviation of sample x. By default computes the cuasi-standard deviation of argument x.
#' @param s sample standard deviation of sample x. Defaults to NULL, if provided, it computes
#' the cuasi-standard deviation.
#' @param n sample size. By default length of argument x.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#' @param alpha a single number in (0,1), significance level.
#' @param plot a logical value indicating whether to display a graph including the test statistic value for the sample, its distribution, the rejection region and p-value.
#' @param lwd single number indicating the line width of the plot.
#'
#' @details The formula interface is applicable when the user provides the sample and also when the user provides the value of the sample characteristics (sample mean, cuasi-standard deviation or sample standard deviation, jointly with the sample size).
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degrees of freedom of the statistic's distribution. NULL for the Normal distribution.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{the sample mean.}
#' \item{null.value}{the value specified by the null.}
#' \item{alternative}{a character string describing the alternative.}
#' \item{method}{a character string indicating the method used.}
#' \item{data.name}{a character string giving the names of the data.}
#' \item{alpha}{the significance level.}
#' \item{dist.name}{a character string indicating the distribution of the test statistic.}
#' \item{statformula}{a character string with the statistic's formula.}
#' \item{reject.region}{a character string with the reject region.}
#' \item{unit}{a character string with the units.}
#'
#' @examples
#' x <- rnorm(50, mean = 4, sd = 2)
#' #unknown sigma
#' Mean.test(x, mu0 = 3.5)
#' Mean.test(mean(x), sc = sd(x),  n = length(x), mu0 = 3.5)
#' #known sigma
#' Mean.test(x, mu0 = 3.5, sigma = 2)
#' @export
Mean.test <- function(x, mu0, sigma = NULL, sc = NULL, s = NULL, n = NULL,
                      alternative = "two.sided", alpha = 0.05, plot = TRUE, lwd = 1) {


  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")

  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")

  if(length(alternative)!=1) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")
  if(!alternative%in%c("two.sided","less","greater")) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")

  if (missing(mu0)) stop("'mu0' must be specified")
  if (length(mu0) != 1L | !is.numeric(mu0) | !is.finite(mu0)) stop("'mu0' must be a single number")

  if ( !((length(alpha) == 1L) && is.finite(alpha) && (alpha > 0) && (alpha < 1)) )
    stop("'alpha' must be a single number between 0 and 1")

  if (length(x) != 1L) {
    xok <- complete.cases(x)
    if (sum(!xok) != 0) warning("Missing values have been removed from 'x'")
    x <- x[xok]
  }


  if (is.null(sigma)) {
    if(length(x)!=1){
      if(!is.null(sc)){warning("As the sample is provided, 'sc' is omitted and computed from the sample")}
      if(!is.null(s)){warning("As the sample is provided, 's' is omitted and computed from the sample")}
      if(!is.null(n)){warning("As the sample is provided, 'n' is omitted and computed from the sample")}
      sc=sd(x);n=length(x);s=(n-1)*sc/n
    }else{
      if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
      if(length(n) != 1L) stop("'n' must be a single positive integer")
      if (n <= 0 | !is.numeric(n) | !is.finite(n)) stop("'n' must be a single positive integer")
      if(!is.wholenumber(n)) stop("'n' must be a single positive integer")
      if(is.null(sc)&is.null(s)){stop("'sc' or 's' needs to be provided when the sample is not")}
      if(!is.null(sc)){
        if(!is.null(s)){warning("As 'sc' is provided, 's' is not used")}
        if(length(sc)!=1){stop("'sc' must be a single positive number")}
        if(any(!is.finite(sc))){stop("'sc' must be a single positive number")}
        if(sc<=0){stop("'sc' must be a single positive number")}
        s=sqrt((n-1)/n)*sc
      }
      if(!is.null(s)){
        if(length(s)!=1){stop("'s' must be a single positive number")}
        if(any(!is.finite(s))){stop("'s' must be a single positive number")}
        if(s<=0){stop("'s' must be a single positive number")}
        if(is.null(sc)){sc=sqrt(n/(n-1))*s}
      }
     }
    Mean.sc.test(x = x, Sc = sc, n = n, mu0 = mu0, alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
  } else {
    if(length(x)!=1){
      if(!is.null(n)){warning("As the sample is provided, 'n' is omitted and computed from the sample")}
      n=length(x)
    }else{
      if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
      if(length(n) != 1L) stop("'n' must be a single positive integer")
      if (n <= 0 | !is.numeric(n) | !is.finite(n)) stop("'n' must be a single positive integer")
      if(!is.wholenumber(n)){stop("'n' must be a single positive integer")}
      if(length(sigma)!=1){stop("'sigma' must be a single positive number")}
        if(any(!is.finite(sigma))){stop("'sigma' must be a single positive number")}
        if(sigma<=0){stop("'sigma' must be a single positive number")}
      }
    if(!is.null(sc))warning("As 'sigma' is provided, 'sc' is not needed")
    if(!is.null(s))warning("As 'sigma' is provided, 's' is not needed")
    Mean.sigma.test(x = x, sigma = sigma, n = n, mu0 = mu0, alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
  }

}
