
diffmean.sigma.test <- function(x, n, y, m, sigmax, sigmay,
                                alternative ,
                                alpha = 0.05, plot = TRUE, lwd = 1) {

  if (length(x) != 1L) bar_x <- mean(x)
    else bar_x <- x
  if (length(y) != 1L) bar_y <- mean(y)
    else bar_y <- y

  DNAME <- paste0(deparse(substitute(x)), ", with sample size ", deparse(substitute(n)), ", and ",
                  deparse(substitute(y)), ", with sample size ", deparse(substitute(m)))
  NVAL <- 0

  # Statistic and pvalue
  STATISTIC <- (bar_x - bar_y) / sqrt(sigmax^2 / n + sigmay^2 / m)
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
          main = 'T follows N(0,1)', axes = FALSE, xlab = "", ylab = "", lwd = lwd)
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
        segments(x0 = qnorm(alpha), y0 = c(-u[4]*0.015, u[4]*0.015),
                 x1 = qnorm(alpha) - u[2]*0.015, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
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
        segments(x0 = qnorm(1 - alpha ), y0 = c(-u[4]*0.015, u[4]*0.015),
                 x1 = qnorm(1 - alpha) + u[2]*0.015, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
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
  METHOD <- "Test for the difference between the means of two Normal populations with known variances"
  DISTNAME <- "\U2208 N(0,1)"
  STATFORMULA <- "(bar.x\U2081 - bar.x\U2082) / sqrt(sigma.x\U2081\U00B2 / n\U2081 + sigma.x\U2082\U00B2 / n\U2082)"
  ESTIMATE <- setNames(bar_x - bar_y, "\U03BC\U2081 - \U03BC\U2082")
  PARAMETER <- NULL
  names(NVAL) <- names(ESTIMATE)
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

