
#' One Sample Variance Test of a Normal Population
#'
#' \code{variance.test} allows to compute hypothesis tests for the variance of a Normal population in both scenarios: known or unknown population mean.
#'
#' @param x a numeric vector containing the sample.
#' @param s a single numeric value corresponding with the sample standard deviation.
#' @param sc a single numeric value corresponding with the cuasi-standard deviation.
#' @param smu if known, a single numeric value corresponding with the estimation of the standard deviation for known population mean.
#' @param mu if known, a single numeric value corresponding with the population mean. Even when the user provides smu, mu is still needed.
#' @param n a single positive integer corresponding with the sample size; not needed if the sample is provided.
#' @param sigma02 a single number corresponding with the variance to test.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#' @param alpha a single number in (0,1), significance level.
#' @param plot a logical value indicating whether to display a graph including the test statistic value for the sample, its distribution, the rejection region and p-value.
#' @param lwd a single number indicating the line width of the plot.
#'
#' @details The formula interface is applicable when the user provides the sample or values of the
#' sample characteristics (cuasi-standard deviation or sample standard deviation).
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degrees of freedom of the statistic's distribution.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{the cuasi-variance.}
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
#' x <- rnorm(50, mean = 1, sd = 2)
#' # unknown population mean
#' variance.test(x, sigma02 = 3.5)
#' variance.test(sc = sd(x), n = 50, sigma02 = 3.5)
#' # known population mean
#' variance.test(x, sigma02 = 3.5, mu = 1)
#' smu <- Smu(x, mu = 1)
#' variance.test(smu = smu, n = 50, sigma02 = 3.5)
#' @export
#'
variance.test <- function(x = NULL, s = NULL, sc = NULL, smu = NULL, mu = NULL, n = NULL,
                          sigma02, alternative = "two.sided", alpha = 0.05,
                          plot = TRUE, lwd = 1) {

  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")

   if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
   if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")

   if(length(alternative)!=1) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")
   if(!alternative%in%c("two.sided","less","greater")) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")


   if (!is.null(x)) {
     if (!is.vector(x)) stop("'x' must be a numeric vector")
     xok <- complete.cases(x)
     if (sum(!xok) != 0) warning("Missing values have been removed from 'x'")
     x <- x[xok]
     if (length(x)<=1) stop("'x' must be a numeric vector")
   }

   if(missing(sigma02)) stop("'sigma02' must be specified")
   if(length(sigma02) != 1L ) stop("'sigma02' must be a single positive number")
   if (!is.numeric(sigma02) | !is.finite(sigma02) | sigma02 <= 0)  stop("'sigma02' must be a single positive number")

   if ( !((length(alpha) == 1L) && is.finite(alpha) && (alpha > 0) && (alpha < 1)) ) stop("'alpha' must be a single number in (0,1)")

   if (is.null(mu) & is.null(smu)) {
      if(is.null(x)&is.null(s)&is.null(sc)){stop("For unknown population mean, either 'x', 's' or 'sc' needs to be provided")}
      if(!is.null(x)){#sample provided
         if(length(x)==1){stop("The sample needs to be at least length two")}
         if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
         if(length(x)==1){stop("The sample needs to be at least length two")}
         if(any(!is.finite(x))){stop("'x' must be a numeric vector")}

         if(!is.null(n)){warning("As the sample is provided, 'n' is omitted and computed from the sample")}
         n=length(x)
         if(!is.null(sc)){warning("As the sample is provided, 'sc' is omitted and computed from the sample")}
         if(!is.null(s)){warning("As the sample is provided, 's' is omitted and computed from the sample")}
         sc=sd(x); s=sqrt((n-1)/n)*sc
      }else{#characteristic provided
         if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
         if(length(n)!=1){stop("'n' must be a single positive integer")}
         if(any(!is.finite(n))){stop("'n' must be a single positive integer")}
         if(!is.wholenumber(n)|n<=0){stop("'n' must be a single positive integer")}
         if(is.null(s)&is.null(sc)){stop("Either 's' or 'sc' needs to be provided.")}
         if(!is.null(sc)){
            if(!is.null(s)){warning("As 'sc' is provided, 's' is not used")}
            if(length(sc)!=1){stop("'sc' must be a single number")}
            if(any(!is.finite(sc))){stop("'sc' must be a single positive number")}
            if(sc<=0){stop("'sc' must be a single positive number")};s=sqrt((n-1)/n)*sc}
         if(!is.null(s)){
            if(length(s)!=1){stop("'s' must be a single positive number")}
            if(any(!is.finite(s))){stop("'s' must be a single positive number")}
            if(s<=0){stop("'s' must be a single positive number")};sc=sqrt(n/(n-1))*s}
      }
     variance.hatmu.test(x = x, sc = sc, n = n, sigma02 = sigma02,
                         alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
   } else {
      if(is.null(x)&is.null(smu)){stop("For known population mean, 'x' or 'smu' needs to be provided")}
      if(!is.null(x)){#sample provided
         if(sum(is.na(x))!=0){x=x[-which(is.na(x))]; warning("Missing values have been removed from 'x'")}
         if(length(x)==1){stop("The sample needs to be at least length two")}
         if(any(!is.finite(x))){stop("'x' must be a numeric vector")}
         if(is.null(smu)&is.null(mu)){stop("'mu' needs to be provided when 'smu' is not")}
         if(!is.null(mu)){
         if(length(mu)!=1){stop("'mu' must be a single number")}
         if(any(!is.finite(mu))){stop("'mu' must be a single number")}
            smu=(1/n)*sum((x-mu)^2)}
         if(!is.null(n)){warning("As the sample is provided, 'n' is omitted and computed from the sample")}
         n=length(x)

      }else{#smu provided
         if(length(smu)!=1){stop("'smu' must be a single positive number")}
         if(any(!is.finite(smu))){stop("'smu' must be a single positive number")}
         if(smu<=0){stop("'smu' must be a single positive number")
            if(is.null(n)){stop("'n' needs to be provided when the sample is not")}
            if(length(n)!=1){stop("'n' must be a single positive integer")}
            if(any(!is.finite(n))){stop("'n' must be a single positive integer")}
            if(!is.wholenumber(n)|n<=0){stop("'n' must be a single positive integer")}}
      }

     if(!is.null(s)) {warning("As the population mean is known, 's' is not used")}
     if(!is.null(sc)){warning("As the population mean is known, 'sc' is not used")}

     variance.mu.test(x = x, n = n, mu = mu, smu = smu, sigma02 = sigma02,
                      alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
   }

}
