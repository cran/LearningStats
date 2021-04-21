
#' Two Sample Mean Test of Normal Populations
#'
#' \code{diffmean.test} allows to compute hypothesis tests about two population means. The difference between the
#' means of two Normal populations is tested in different scenarios: known or unknown variance,
#' variances assumed equal or different and paired or independent populations.
#'
#' @param x1 a numeric vector of data values or, if single number, estimated mean.
#' @param x2 a numeric vector of data values or, if single number, estimated mean.
#' @param sigma1 if known, a single numeric value corresponding with one of the population standard deviation.
#' @param sigma2 if known, a single numeric value corresponding with the other population standard deviation.
#' @param sc1 cuasi-standard deviation of sample x1. By default computes the cuasi-standard deviation of argument \code{x1}.
#' @param sc2 cuasi-standard deviation of sample x2. By default computes the cuasi-standard deviation of argument \code{x2}.
#' @param s1 sample standard deviation of sample x1. Defaults to NULL, if provided, it computes
#' the cuasi-standard deviation.
#' @param s2 sample standard deviation of sample x2. Defaults to NULL, if provided, it computes
#' the cuasi-standard deviation.
#' @param n1 sample size of \code{x1}. By default length of argument \code{x1}.
#' @param n2 sample size of \code{x2}. By default length of argument \code{x2}.
#' @param var.equal a logical indicating whether to treat the two variances as being equal. Defaults to FALSE.
#' @param paired a logical indicating whether the samples are paired. Defaults to FALSE, if TRUE,
#' then both x1 and x2 must be the same length.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#' @param alpha single number in (0,1), corresponding with the significance level.
#' @param plot a logical value indicating whether to display a graph including the test statistic value for the sample, its distribution, the rejection region and p-value.
#' @param lwd single number indicating the line width of the plot.
#'
#' @details If sigma1 and sigma2 are given, known population variances formula is applied; the unknown one is used in other case.
#'
#' If paired is TRUE then both x1 and x2 must be specified and their sample sizes must be the same. If paired is null, then it is assumed to be FALSE.
#'
#' For var.equal=TRUE, the formula of the pooled variance is \eqn{\frac{(n1-1)sc1^2+(n2-1)sc2^2}{n1+n2-2}}.
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degrees of freedom of the statistic's distribution. NULL for the Normal distribution.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{the estimated difference in means.}
#' \item{null.value}{the value specified by the null.}
#' \item{alternative}{a character string describing the alternative.}
#' \item{method}{a character string indicating the method used.}
#' \item{data.name}{a character string giving the names of the data.}
#' \item{alpha}{the significance level.}
#' \item{dist.name}{a character string indicating the distribution of the test statistic.}
#' \item{statformula}{a character string with the statistic's formula.}
#' \item{reject.region}{a character string with the reject region.}
#'
#' @examples
#' x1 <- rnorm(40, mean = 1.5, sd = 2)
#' x2 <- rnorm(60, mean = 2, sd = 2)
#' #equal variances
#' diffmean.test(x1, x2, var.equal = TRUE)
#' diffmean.test(mean(x1), mean(x2),
#'               sc1 = sd(x1), sc2 = sd(x2),
#'               n1 = 40, n2 = 60, var.equal = TRUE)
#' x3 <- rnorm(60, mean = 2, sd = 1.5)
#' #different variances
#' diffmean.test(x1, x3)
#' #known standard deviation
#' diffmean.test(x1, x3, sigma1 = 2, sigma2 = 1.5)
#' x4 <- x1 + rnorm(40, mean = 0, sd = 0.1)
#' #paired samples
#' diffmean.test(x1, x4, paired = TRUE)
#' @export
diffmean.test <- function(x1, x2, sigma1 = NULL, sigma2 = NULL, sc1 = NULL, sc2 = NULL,
                          s1 = NULL, s2 = NULL, n1=NULL, n2 =NULL, var.equal = FALSE,
                          paired = FALSE, alternative ="two.sided",
                          alpha = 0.05, plot = TRUE, lwd = 1) {

  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")
  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a single positive integer")

  if(length(paired)!=1){stop("'paired' must be a single logical value")}
  if (!is.logical(paired)|is.na(paired)) stop("'paired' must be a single logical value")
  if(length(var.equal)!=1){stop("'var.equal' must be a single logical value")}
  if (!is.logical(var.equal)|is.na(var.equal)) stop("'var.equal' must be a single logical value")
  if (!is.vector(x1)) stop("'x1' must be a numeric vector")
  if (!is.vector(x2)) stop("'x2' must be a numeric vector")

  if ( !((length(alpha) == 1L) && any(is.finite(alpha)) && (alpha > 0) && (alpha < 1)) )
    stop("'alpha' must be a single number between 0 and 1")

  if(length(alternative)!=1) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")
  if(!alternative%in%c("two.sided","less","greater")) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")

  # ---------------------------
  if(!is.null(n1)){if(length(n1) != 1L)stop("'n1' must be a single positive integer")}
  if(!is.null(n2)){if (length(n2) != 1L)stop("'n2' must be a single positive integer")}

  if(!is.null(n1)){
    if (n1 == 1L) stop("Sample size 'n1' must be bigger than 1")
    if (n1 <= 0 | !is.numeric(n1) | any(!is.finite(n1)))
      stop("'n1' must be a single positive integer")
    if(!is.wholenumber(n1)){stop("'n1' must be a single positive integer")}
  }
  if(!is.null(n2)){
    if (n2 == 1L) stop("Sample size 'n2' must be bigger than 1")
    if (n2 <= 0 | !is.numeric(n2) | any(!is.finite(n2)))
      stop("'n2' must be a single positive integer")
    if(!is.wholenumber(n2)){stop("'n2' must be a single positive integer")}
  }

  if ( sum(c(is.null(sigma1), is.null(sigma2))) == 1 & paired==FALSE) stop("Both 'sigma1' and 'sigma2' must be specified")

  if (!is.null(s1)) {
    if (length(s1) != 1L | !is.numeric(s1) | any(!is.finite(s1)) | any(s1 <= 0)) stop("'s1' must be a single positive number")
    sc1 <- sqrt(n1 / (n1 - 1)) * s1
  }
  if (!is.null(s2)) {
    if (length(s2) != 1L | !is.numeric(s2) | any(!is.finite(s2)) | any(s2 <= 0)) stop("'s2' must be a single positive number")
    sc2 <- sqrt(n2 / (n2 - 1)) * s2
  }

  if(!is.null(sc1)){if (is.na(sc1) | length(sc1) != 1L | !is.numeric(sc1) | any(!is.finite(sc1)) | any(sc1 <= 0)) stop("'sc1' must be a single positive number")}
  if(!is.null(sc2)){if (is.na(sc2) | length(sc2) != 1L | !is.numeric(sc2) | any(!is.finite(sc2)) | any(sc2 <= 0)) stop("'sc2' must be a positive number")}

  if (!is.null(sigma1)) {
    if (length(sigma1) != 1L | !is.numeric(sigma1) | any(!is.finite(sigma1)) | any(sigma1 <= 0))
      stop("'sigma1' must be a single positive number")
  }
  if (!is.null(sigma2)) {
    if (length(sigma2) != 1L | !is.numeric(sigma2) | any(!is.finite(sigma2)) | any(sigma2 <= 0))
      stop("'sigma2' must be a single positive number")
  }

  # ---------------------------
  # NA handling ---------------
  if (paired) {
    if (length(x1) == 1L | length(x2) == 1L) stop("In paired populations both samples are needed")
    if (length(x1) != length(x2)) stop('In paired populations sample sizes must be the same')
    ok <- complete.cases(x1, x2)
    x1 <- x1[ok]
    x2 <- x2[ok]
    if (length(x1) <= 1L | !is.numeric(x1) | any(!is.finite(x1))) stop("'x1' must be a numeric vector")
    if (length(x2) <= 1L | !is.numeric(x2) | any(!is.finite(x2))) stop("'x2' must be a numeric vector")
  } else {
    if (length(x1) != 1L) {
      x1ok <- complete.cases(x1)
      if (sum(!x1ok) != 0) warning("Missing values have been removed from 'x1'")
      x1 <- x1[x1ok]
#      n1 <- length(x1)
#      if (n1 <= 1L | !is.numeric(x1) | any(!is.finite(x1))) stop("'x1' must be a numeric vector")
    }
    if (length(x2) != 1L) {
      x2ok <- complete.cases(x2)
      if (sum(!x2ok) != 0) warning("Missing values have been removed from 'x2'")
      x2 <- x2[x2ok]
#      n2 <- length(x2)
#      if (n2 <= 1L | !is.numeric(x2) | any(!is.finite(x2))) stop("'x1' must be a numeric vector")
    }
  }

  # ---------------------------
  if(paired==TRUE){
    if(!is.null(sc1)){warning("As sample1 is provided, 'sc1' is omitted and computed from the sample")}
    if(!is.null(s1)){warning("As sample1 is provided, 's1' is omitted and computed from the sample")}
    if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
    if(!is.null(sc2)){warning("As sample2 is provided, 'sc2' is omitted and computed from the sample")}
    if(!is.null(s2)){warning("As sample2 is provided, 's2' is omitted and computed from the sample")}
    if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
    if(!is.null(sigma1)){warning("'sigma1' is not needed for paired samples")}
    if(!is.null(sigma2)){warning("'sigma2' is not needed for paired samples")}
  }else if(paired==FALSE){
    if(!is.null(sigma1)|!is.null(sigma2)){
      if(is.null(sigma1) | is.null(sigma2)){stop("'sigma1' and 'sigma2' must be specified for both populations")}
      if (any(!is.finite(sigma1))){stop("'sigma1' must be numeric")}
      if(length(sigma1)!=1){stop("'sigma1' must be a single positive number")}
      if (any(!is.finite(sigma2))){stop("'sigma2' must be numeric")}
      if(length(sigma2)!=1){stop("'sigma2' must be a single positive number")}
      if(sigma1<=0){stop("'sigma1' must be a single positive number")}
      if(sigma2<=0){stop("'sigma2' must be a single positive number")}
      if(!is.null(s1)){warning("As the population variance 'sigma1' is known, 's1' is not used")}
      if(!is.null(sc1)){warning("As the population variance 'sigma1' is known, 'sc1' is not used")}
      if(!is.null(s2)){warning("As the population variance 'sigma2' is known, 's2' is not used")}
      if(!is.null(sc2)){warning("As the population variance 'sigma2' is known, 'sc2' is not used")}
      if(length(x1)!=1){
        if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
        n1=length(x1)}
      if(length(x2)!=1){
        if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
        n2=length(x2)}
    }else{
      if(length(x1)!=1){
        if(!is.null(sc1)){warning("As sample1 is provided, 'sc1' is omitted and computed from the sample")}
        if(!is.null(s1)){warning("As sample1 is provided, 's1' is omitted and computed from the sample")}
        if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
        sc1=sd(x1);n1=length(x1);s1=(n1-1)*sc1/n1
        }else{
        if(is.null(n1)) stop("'n1' needs to be provided when the sample1 is not")
        if(!is.null(sc1)&!is.null(s1))warning("As 'sc1' is provided, 's1' is not used")
      }
      if(length(x2)!=1){
        if(!is.null(sc2)){warning("As sample2 is provided, 'sc2' is omitted and computed from the sample")}
        if(!is.null(s2)){warning("As sample2 is provided, 's2' is omitted and computed from the sample")}
        if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
        sc2=sd(x2);n2=length(x2);s2=(n2-1)*sc2/n2
      }else{
        if(is.null(n2)) stop("'n2' needs to be provided when the sample2 is not")
        if(!is.null(sc2)&!is.null(s2))warning("As 'sc2' is provided, 's2' is not used")
      }
    }
  }




  # ---------------------------

  if (is.null(sigma1) & isFALSE(paired)) {
    if (var.equal) {
      diffmean.equal.test(x = x1, n = n1, y = x2, m = n2, Scx = sc1, Scy = sc2,
                          alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
    } else {
      diffmean.neq.test(x = x1, n = n1, y = x2, m = n2, Scx = sc1, Scy = sc2,
                        alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
    }
  } else if (isTRUE(paired)) {
    diffmean.paired.test(x = x1, y = x2, alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
  } else if (!is.null(sigma1)) {
    diffmean.sigma.test(x = x1, n = n1, y = x2, m = n2, sigmax = sigma1, sigmay = sigma2,
                        alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
  } else {
    stop("Invalid arguments to perform the test")
  }



}
