
#' Two Sample Proportion Test
#'
#' \code{diffproportion.test} allows to compute hypothesis tests about two population proportions.
#'
#' @param x1 a single numeric value corresponding with either the proportion estimate or the number of successes of one of the samples.
#' @param x2 a single numeric value corresponding with either the proportion estimate or the number of successes of the other sample.
#' @param n1 a single positive integer value corresponding with one sample size.
#' @param n2 a single positive integer value corresponding with the other sample size.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#' @param alpha single number in (0,1) corresponding with the significance level.
#' @param plot a logical value indicating whether to display a graph including the test statistic value for the sample, its distribution, the rejection region and p-value.
#' @param lwd single number indicating the line width of the plot.
#'
#' @details Counts of successes and failures must be nonnegative and hence not greater than the corresponding
#' numbers of trials which must be positive. All finite counts should be integers. If the number of successes
#' is given, then the proportion estimate is computed.
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the sample size \code{n1}.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{the difference of sample proportions.}
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
#' x1 <- rbinom(1, 120, 0.6)
#' x2 <- rbinom(1, 100, 0.6)
#' diffproportion.test(x1 = x1, x2 = x2, n1 = 120, n2 = 100)
#' diffproportion.test(x1 = 0.6, x2 = 0.65, n1 = 120, n2 = 100)
#' @export
diffproportion.test <- function(x1, x2, n1, n2, alternative = "two.sided",
                                alpha = 0.05, plot = TRUE, lwd = 1) {

  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")

  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")

  if ( !((length(alpha) == 1L) && is.finite(alpha) && (alpha > 0) && (alpha < 1)) )
    stop("'alpha' must be a single number between 0 and 1")
  if(length(x1)!=1){stop("'x1' must be a single positive number")}
  if (missing(x1)| x1 <= 0 | !is.numeric(x1) | !is.finite(x1))
    stop("'x1' must be a single positive number")
  if(length(x2)!=1){stop("'x2' must be a single positive number")}
  if (missing(x2) | x2 <= 0 | !is.numeric(x2) | !is.finite(x2))
    stop("'x2' must be a single positive number")
  if(length(n1)!=1){stop("'n1' must be a single positive integer")}
  if (missing(n1)| n1 <= 0 | !is.numeric(n1) | !is.finite(n1))
    stop("'n1' must be a single positive integer")
  if(!is.wholenumber(n1)){stop("'n1' must be a single positive integer")}
  if(length(n2)!=1){stop("'n2' must be a single positive integer")}
  if (missing(n2) | n2 <= 0 | !is.numeric(n2) | !is.finite(n2))
    stop("'n2' must be a single positive integer")
  if(!is.wholenumber(n2)){stop("'n2' must be a single positive integer")}
  if (x1 > n1 | x2 > n2) stop("The sample size 'n' must be larger than 'x'")
  if (n1 < 30 | n2 < 30) warning("The sample size is small for the approximation used in the method, it should be n > 30")

  if(length(alternative)!=1) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")
  if(!alternative%in%c("two.sided","less","greater")) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")


  if (x1 < 1) px1 <- x1
    else px1 <- x1 / n1

  if (x2 < 1) px2 <- x2
    else px2 <- x2 / n2

  hat_p <- (n1 * px1 + n2 * px2) / (n1 + n2)
  DNAME <- paste0(deparse(substitute(x1)), " out of ", deparse(substitute(n1)), " and ",
                  deparse(substitute(x2)), " out of ", deparse(substitute(n2)))
  NVAL <- 0

  # Statistic and pvalue
  STATISTIC <- (px1 - px2) / sqrt(hat_p * (1 - hat_p) * (1/n1 + 1/n2))
  if (alternative == "two.sided")
    PVALUE <- 2 * pnorm(abs(STATISTIC), lower.tail = FALSE)
  else
    PVALUE <- pnorm(STATISTIC, lower.tail = (alternative == "less"))
  # Reject Region
  RR <- paste0("RR = ", switch(alternative,
                               two.sided = paste0("(-\U221E, ", round(qnorm(alpha/2), 5), "] U [",
                                                  round(qnorm(1 - alpha/2), 5), ", +\U221E)"),
                               greater = paste0("[", round(qnorm(1 - alpha), 5), ", +\U221E)"),
                               less = paste0("(-\U221E, ", round(qnorm(alpha), 5), "]")))

  # Plot
  if (plot) {
    ## Plot statistic distribution
    curve(dnorm(x), from = min(-3, -abs(STATISTIC) - 0.1), to = max(3, abs(STATISTIC) + 0.1),
          main = 'T ~ N(0,1)', axes = FALSE, xlab = "", ylab = "", lwd = lwd)
    u <- par("usr") # x0, x1, y0, y1
    rect(u[1], 0, u[2], u[4])
    axis(2)
    legend("topright", c("p-value", "RR"), bty = "n", pch = c(22,NA), lty = c(NA,1), lwd = c(1,2),
           col = c("blue", "red"), pt.bg = adjustcolor('blue', alpha.f = 0.25), pt.cex = 2, seg.len = 1, cex = 1)
    if (alternative == "two.sided") {
      abline(h = 0, lwd = lwd + 1)
      lines(c(u[1], qnorm(alpha / 2)), c(0,0), col = "red", lwd = lwd + 1)
      lines(c(qnorm(1 - alpha / 2), u[2]), c(0,0), col = "red", lwd = lwd + 1)
      axis(1, pos = 0, col = NA, col.ticks = 1,
           at     = c(0, STATISTIC, -1*STATISTIC),
           labels = c(0, expression('T'[obs]), expression(-'T'[obs])))
      segments(x0 = c(qnorm(alpha / 2), qnorm(1 - alpha / 2)), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
      segments(x0 = c(qnorm(alpha / 2), qnorm(1 - alpha / 2)), y0 = rep(c(-u[4]*0.015, u[4]*0.015), each = 2),
               x1 = c(qnorm(alpha / 2) - u[2]*0.015, qnorm(1 - alpha / 2) + u[2]*0.015),
               rep(c(-u[4]*0.015, u[4]*0.015), each = 2), col = "red", lwd = lwd + 1)
      if ((abs(STATISTIC) - qnorm(1 - alpha / 2)) > 0.3) {
        axis(1, pos = 0, col = NA, col.ticks = NA,
             at     = c(qnorm(alpha / 2), qnorm(1 - alpha / 2)),
             labels = c(expression(-'z'[1-alpha/2]), expression('z'[1-alpha/2])))
        mtext("=", side = 1, line = 1.6, at = c(qnorm(alpha / 2), qnorm(1 - alpha / 2)), las = 2)
        mtext(round(qnorm(alpha / 2), 2), side = 1, line = 2.5, at = qnorm(alpha / 2))
        mtext(round(qnorm(1 - alpha / 2), 2), side = 1, line = 2.5, at = qnorm(1 - alpha / 2))
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic right tail
        segments(x0 = qnorm(1 - PVALUE / 2), y0 = 0,
                 x1 = qnorm(1 - PVALUE / 2), y1 = dnorm(qnorm(1 - PVALUE / 2)),
                 col = 'blue', lwd = 1)
        x_vector <- seq(qnorm(1 - PVALUE / 2), 4, length = 100)
        y_vector <- dnorm(x_vector)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = qnorm(1 - PVALUE / 2), las = 2)
        mtext(round(qnorm(1 - PVALUE / 2), 2), side = 1, line = 2.5, at = qnorm(1 - PVALUE / 2))

        # Statistic left tail
        segments(x0 = qnorm(PVALUE / 2), y0 = 0,
                 x1 = qnorm(PVALUE / 2), y1 = dnorm(qnorm(PVALUE / 2)),
                 col = 'blue', lwd = 1)
        x_vector <- seq(-4, qnorm(PVALUE / 2), length = 100)
        y_vector <- dnorm(x_vector)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = qnorm(PVALUE / 2), las = 2)
        mtext(round(qnorm(PVALUE / 2), 2), side = 1, line = 2.5, at = qnorm(PVALUE / 2))
      }

    } else {

      if (alternative == "less") {
        abline(h = 0, lwd = lwd + 1)
        lines(c(u[1], qnorm(alpha)), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at     = c(0, STATISTIC),
             labels = c(0, expression('T'[obs])))
        segments(x0 = qnorm(alpha), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qnorm(alpha), y0 = c(-u[4]*0.015, u[4]*0.015), x1 = qnorm(alpha) - u[2]*0.015,
                 c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qnorm(alpha)) > 0.3) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qnorm(alpha), labels = expression(-'z'[1-alpha]))
          mtext("=", side = 1, line = 1.6, at = qnorm(alpha), las = 2)
          mtext(round(qnorm(alpha), 2), side = 1, line = 2.5, at = qnorm(alpha))
        }

      } else {
        abline(h = 0, lwd = lwd + 1)
        lines(c(qnorm(1 - alpha), u[2]), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at       = c(0, STATISTIC),
             labels   = c(0, expression('T'[obs])))
        segments(x0 = qnorm(1 - alpha), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qnorm(1 - alpha ), y0 = c(-u[4]*0.015, u[4]*0.015), x1 = qnorm(1 - alpha) + u[2]*0.015,
                 c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qnorm(1 - alpha)) > 0.3) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qnorm(1 - alpha), labels = expression('z'[1-alpha]))
          mtext("=", side = 1, line = 1.6, at = qnorm(1 - alpha), las = 2)
          mtext(round(qnorm(1 - alpha), 2), side = 1, line = 2.5, at = qnorm(1 - alpha))
        }
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic
        segments(x0 = STATISTIC, y0 = 0, x1 = STATISTIC, y1 = dnorm(STATISTIC),
                 col = 'blue', lwd = 1)
        if (alternative == "less") {
          x_vector <- seq(-4, STATISTIC, length = 100)
        } else {
          x_vector <- seq(STATISTIC, 4, length = 100)
        }
        y_vector <- dnorm(x_vector)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
        mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
      }
    }
  }

  ##---------------------------------------------
  METHOD <- "Test for a proportion difference"
  DISTNAME <- "~ N(0,1)"
  STATFORMULA <- "(hat.px\U2081 - hat.px\U2082) / sqrt(hat.pw * (1-hat.pw) * (1/n\U2081 + 1/n\U2082)),
                  \t with hat.pw = (n\U2081 * hat.px\U2081 + n\U2082 * hat.px\U2082) / (n\U2081 + n\U2082)"
  ESTIMATE <- setNames(px1 - px2, "px\U2081 - px\U2082")
  PARAMETER <- n1
  names(NVAL) <- names(ESTIMATE)
  names(PARAMETER) <- "n1"
  names(STATISTIC) <- "T"
  RVAL <- list(
    statistic = STATISTIC,
    parameter = PARAMETER,
    p.value = as.numeric(PVALUE),
    estimate = ESTIMATE,
    null.value = NVAL,
    alternative = alternative,
    method = METHOD,
    data.name = DNAME,
    alpha = alpha,
    dist.name = DISTNAME,
    statformula = STATFORMULA,
    reject.region = RR
  )
  class(RVAL) <- c("lstest", "htest")
  return(RVAL)
}
