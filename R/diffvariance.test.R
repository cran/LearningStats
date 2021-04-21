
#' Two Sample Variance Test of Normal Populations
#'
#' \code{diffvariance.test} allows to compute hypothesis tests about two population variances in both scenarios: known and unknown population mean.
#'
#' @param x1 a numeric vector containing the sample of one population.
#' @param x2 a numeric vector containing the sample of the other population.
#' @param s1 a single numeric value corresponding with the sample standard deviation of the first sample.
#' @param s2 a single numeric value corresponding with the sample standard deviation of the second sample.
#' @param sc1 a single numeric value corresponding with the cuasi-standard deviation of the first sample.
#' @param sc2 a single numeric value corresponding with the cuasi-standard deviation of the second sample.
#' @param smu1 if known, a single numeric value corresponding with the estimation of the standard deviation of the first sample.
#' @param smu2 if known, a single numeric value corresponding with the estimation of the standard deviation of the second sample.
#' @param mu1 if known, a single numeric  corresponding with the mean of one population.
#' @param mu2 if known, a single numeric value corresponding with the mean of the other population.
#' @param n1 a single positive integer corresponding with the size of one sample.
#' @param n2 a single positive integer conrresponding with the size of the other sample.
#' @param n1 a single number indicating the sample size of \code{x1}. By default length of
#' argument \code{x1}.
#' @param n2 a single number indicating the sample size of \code{x2}. By default length of
#' argument \code{x2}.
#' @param alternative a character string specifying the alternative hypothesis, must be one
#' of "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#' @param alpha single number between 0 and 1, significance level.
#' @param plot a logical value indicating whether to display a graph including the test statistic value for the sample, its distribution, the rejection region and p-value.
#' @param lwd single number indicating the line width of the plot.
#'
#' @details The formula interface is applicable when the user provides the sample(s) or values
#' of the sample characteristics (cuasi-standard deviation or sample standard deviation).
#' When \code{mu1} and \code{mu2} or \code{smu1} and \code{smu2} are provided, the function performs
#' the procedure with known population means.
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degrees of the freedom of the F distribution of the test statistic.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{the ratio of the cuasi-variances of \code{x1} and \code{x2}.}
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
#' x1 <- rnorm(40, mean = 1, sd = 2)
#' x2 <- rnorm(60, mean = 2, sd = 1.5)
#' # unknown population mean
#' diffvariance.test(x1, x2)
#' diffvariance.test(x1, sc2 = sd(x2), n2 = length(x2))
#' diffvariance.test(sc1 = sd(x1), sc2 = sd(x2), n1 = length(x1), n2 = length(x2))
#' # known population mean
#' diffvariance.test(x1, x2, mu1 = 1, mu2 = 2)
#' smu1 <- Smu(x1, mu = 1); smu2 <- Smu(x2, mu = 2)
#' diffvariance.test(smu1 = smu1, smu2 = smu2, n1 = length(x1), n2 = length(x2))
#' @export
diffvariance.test <- function(x1 = NULL, x2 = NULL, s1 = NULL, s2 = NULL, sc1 = NULL, sc2 = NULL,
                              smu1 = NULL, smu2 = NULL, mu1 = NULL, mu2 = NULL, n1 = NULL,
                              n2 = NULL, alternative = "two.sided",
                              alpha = 0.05, plot = TRUE, lwd = 1) {

  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")

  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")

  if ( !((length(alpha) == 1L) && is.finite(alpha) && (alpha > 0) && (alpha < 1)) )
    stop("'alpha' must be a single number between 0 and 1")

  if(length(alternative)!=1) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")
  if(!alternative%in%c("two.sided","less","greater")) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")


  # NA handling ---------------
  if (!is.null(x1)) {
    if (!is.vector(x1)) stop("'x1' must be a numeric vector")
    x1ok <- complete.cases(x1)
    if (sum(!x1ok) != 0) warning("Missing values have been removed from 'x1'")
    x1 <- x1[x1ok]
    if (length(x1) <= 1L | !is.numeric(x1)  | any(!is.finite(x1))) stop("'x1' must be a numeric vector")
    if(!is.null(n1)){"As the sample1 is provided, 'n1' is omitted and computed from the sample"}
#    n1=length(x1)
  }
  if (!is.null(x2)) {
    if (!is.vector(x2)) stop("'x2' must be a vector")
    x2ok <- complete.cases(x2)
    if (sum(!x2ok) != 0) warning("Missing values have been removed from 'x2'")
    x2 <- x2[x2ok]
    if (length(x2) <= 1L | !is.numeric(x2) | any(!is.finite(x2))) stop("'x2' must be a numeric vector")
    if(!is.null(n2)){"As the sample2 is provided, 'n2' is omitted and computed from the sample"}
#    n2=length(x2)
  }
  # ---------------------------

  if(!is.null(mu1)|!is.null(smu1)){
    if(is.null(mu2)&is.null(smu2))stop("'mu' or 'smu' must be specified for both populations")
  }
  if(!is.null(mu2)|!is.null(smu2)){
    if(is.null(mu1)&is.null(smu1))stop("'mu' or 'smu' must be specified for both populations")
  }

  if (is.null(smu1) & is.null(mu1)) { #unkown means
     if(!is.null(x1)){#sample 1 is provided
      if(!is.null(s1)){warning("As sample1 is provided, 's1' is omitted and computed from the sample")}
      if(!is.null(sc1)){warning("As sample1 is provided, 'sc1' is omitted and computed from the sample")}
      if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
      n1=length(x1)
      sc1=sd(x1)
      s1=(n1-1)*sc1/n1
    }else{#sample 1 is NOT provided
      if(length(n1)!=1)stop("'n1' must be a single positive integer")
      if( n1 <= 1 | !is.numeric(n1) | !is.finite(n1)) stop("'n1' must be a single positive integer")
      if(!is.wholenumber(n1)){stop("'n1' must be a single positive integer")}
      if(is.null(sc1) & is.null(s1)){stop("When the population mean is unknown, 's1' or 'sc1' needs to be provided when the sample1 is not")}
      if (!is.null(sc1)) {
        if (length(sc1) != 1L | !is.numeric(sc1) | any(!is.finite(sc1)) | any(sc1 <= 0)) stop("'sc1' must be a single positive number")
      }
      if (!is.null(s1)) {
        if (length(s1) != 1L | !is.numeric(s1) | any(!is.finite(s1)) | any(s1 <= 0)) stop("'s1' must be a single positive number")
      }
    }
    if(!is.null(x2)){#sample2 is provided
      if(!is.null(s2)){warning("As sample2 is provided, 's2' is omitted and computed from the sample")}
      if(!is.null(sc2)){warning("As sample2 is provided, 'sc2' is omitted and computed from the sample")}
      if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
      n2=length(x2)
      sc2=sd(x2)
      s2=(n2-1)*sc2/n2
    }else{#sample 2 is NOT provided
      if(length(n2)!=1)stop("'n2' must be a single positive integer")
      if (n2 <= 1 | !is.numeric(n2) | !is.finite(n2) ) stop("'n2' must be a single positive integer")
      if(!is.wholenumber(n2)){stop("'n2' must be a single positive integer")}
      if(is.null(sc2) & is.null(s2)){stop("When the population mean is unknown, 's2' or 'sc2' needs to be provided when the sample2 is not")}
      if (!is.null(sc2)) {
        if (length(sc2) != 1L | !is.numeric(sc2) | any(!is.finite(sc2)) | any(sc2 <= 0)) stop("'sc2' must be a single positive number")
      }
      if (!is.null(s2)) {
        if (length(s2) != 1L | !is.numeric(s2) | any(!is.finite(s2)) | any(s2 <= 0)) stop("'s2' must be a single positive number")
      }
    }

    diffvariance.unknown.test(x1 = x1, x2 = x2, sc1 = sc1, sc2 = sc2, n1 = n1, n2 = n2,
                              alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
  } else {
    if (!is.null(mu1) & is.null(x1)& is.null(smu1)) stop("If 'mu1' is specified, the sample 'x1' or 'smu1' must be supplied")
    if (!is.null(mu2) & is.null(x2)& is.null(smu2)) stop("If 'mu2' is specified, the sample 'x2' or 'smu2' must be supplied")

    if (!is.null(smu1)) {
      if (length(smu1) != 1L | !is.numeric(smu1) | any(!is.finite(smu1)) | any(smu1 <= 0)) stop("'smu1' must be a single positive number")
    }
    if (!is.null(smu2)) {
      if (length(smu2) != 1L | !is.numeric(smu2) | any(!is.finite(smu2)) | any(smu2 <= 0)) stop("'smu2' must be a single positive number")
    }
    if (!is.null(mu1)) {
      if (length(mu1) != 1L | !is.numeric(mu1) | any(!is.finite(mu1))) stop("'mu1' must be a single positive number")
    }
    if (!is.null(mu2)) {
      if (length(mu2) != 1L | !is.numeric(mu2) | any(!is.finite(mu2))) stop("'mu2' must be a single positive number")
    }

    if(!is.null(x1)){
      if(!is.null(n1)){warning("As sample1 is provided, 'n1' is omitted and computed from the sample")}
      n1=length(x1)
    }else{#sample 1 is NOT provided
      if(length(n1)!=1)stop("'n1' must be a single positive integer")
      if( n1 <= 1 | !is.numeric(n1) | !is.finite(n1)) stop("'n1' must be a single positive integer")
      if(!is.wholenumber(n1)){stop("'n1' must be a single positive integer")}
    }
    if(!is.null(x2)){
      if(!is.null(n2)){warning("As sample2 is provided, 'n2' is omitted and computed from the sample")}
      n2=length(x2)
    }else{#sample 2 is NOT provided
      if(length(n2)!=1)stop("'n2' must be a single positive integer")
      if (n2 <= 1 | !is.numeric(n2) | !is.finite(n2)) stop("'n2' must be a single positive integer")
      if(!is.wholenumber(n2)){stop("'n2' must be a single positive integer")}
    }

    if(!is.null(s1)){warning("As the population mean is known, 's1' is not used")}
    if(!is.null(sc1)){warning("As the population mean is known, 'sc1' is not used")}
    if(!is.null(s2)){warning("As the population mean is known, 's2' is not used")}
    if(!is.null(sc2)){warning("As the population mean is known, 'sc2' is not used")}

    diffvariance.mu.test(x1 = x1, x2 = x2, n1 = n1, n2 = n2, mu1 = mu1, mu2 = mu2, smu1 = smu1,
                         smu2 = smu2, alternative = alternative, alpha = alpha, plot = plot, lwd = lwd)
  }

}

