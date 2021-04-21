
#' Large Sample Test for a Population Proportion
#'
#' \code{proportion.test} allows to compute a hypothesis test for a population proportion.
#'
#' @param x a positive number indicating the counts of successes or, if number
#' between 0 and 1, probability of success.
#' @param n a single positive integer corresponding with the sample size.
#' @param p0 a positive number in (0,1) corresponding with the proportion to test.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#' @param alpha a single number in (0,1) corresponding with significance level.
#' @param plot a logical value indicating whether to display a graph including the test statistic value for the sample, its distribution, the rejection region and p-value.
#' @param lwd a single number indicating the line width of the plot.
#'
#' @details Counts of successes and failures must be nonnegative and hence not greater
#' than the corresponding numbers of trials which must be positive. All finite counts
#' should be integers. If the number of successes is given, then the proportion estimate is computed.
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the sample size \code{n}.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{the sample proportion.}
#' \item{null.value}{the value of \code{p0} specified by the null.}
#' \item{alternative}{a character string describing the alternative.}
#' \item{method}{a character string indicating the method used.}
#' \item{data.name}{a character string giving the names of the data.}
#' \item{alpha}{the significance level.}
#' \item{dist.name}{a character string indicating the distribution of the test statistic.}
#' \item{statformula}{a character string with the statistic's formula.}
#' \item{reject.region}{a character string with the reject region.}
#'
#' @examples
#' x <- rbinom(1, 120, 0.6)
#' proportion.test(x, 120, 0.5, alternative = "greater")
#' proportion.test(0.6, 120, 0.5, alternative = "greater")
#' @export
proportion.test <- function(x, n, p0, alternative = "two.sided",
                            alpha = 0.05, plot = TRUE, lwd = 1) {


  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")

  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a single positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a single positive integer")
  if ( !((length(alpha) == 1L) && is.finite(alpha) && (alpha > 0) && (alpha < 1)) )
    stop("'alpha' must be a single number in (0,1)")
  if(length(x) != 1L)stop("'x' must be a single positive number")
  if (missing(x) | x <= 0 | !is.numeric(x) | !is.finite(x))
    stop("'x' must be a single positive number")
  if(length(n) != 1L) stop("'n' must be a single positive integer")
  if (missing(n) | n <= 0 | !is.numeric(n) | !is.finite(n)) stop("'n' must be a single positive integer")
  if(!is.wholenumber(n)){stop("'n' must be a single positive integer")}
  if(length(p0) != 1L) stop("'p0' must be a single number in (0,1)")
  if ((p0 <= 0) | (p0 >= 1))
    stop("'p0' must be a single number in (0,1)")
  if (x > n) stop("The sample size 'n' must be larger than 'x'")
  if (n < 30) warning("The sample size is small for the approximation used in the method, it should be n > 30")

  if (x < 1) {
    hat_p <- x
    DNAME <- paste0(hat_p * n, " out of ", deparse(substitute(n)),
                    ", null probability ", deparse(substitute(p0)))
  } else {
    hat_p <- x / n
    DNAME <- paste0(deparse(substitute(x)), " out of ", deparse(substitute(n)),
                    ", null probability ", deparse(substitute(p0)))
  }

  if(length(alternative)!=1) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")
  if(!alternative%in%c("two.sided","less","greater")) stop("The argument 'alternative' must be 'two.sided','less' or 'greater'")

  NVAL <- p0

  # Statistic and pvalue
  STATISTIC <- (hat_p - p0) / sqrt(p0 * (1 - p0) / n)
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
  METHOD <- "Test for a proportion"
  DISTNAME <- "~ N(0,1)"
  STATFORMULA <- "(hat.p - p\U2080) / sqrt(p\U2080 * (1 - p\U2080) / n)"
  ESTIMATE <- setNames(hat_p, "p")
  PARAMETER <- n
  names(NVAL) <- names(ESTIMATE)
  names(PARAMETER) <- "n"
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



