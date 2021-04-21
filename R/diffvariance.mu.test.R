
diffvariance.mu.test <- function(x1 = NULL, x2 = NULL, n1 = NULL, n2 = NULL, mu1 = NULL, mu2 = NULL,
                                 smu1 = NULL, smu2 = NULL, alternative = alternative,
                                 alpha = 0.05, plot = TRUE, lwd = 1) {


  if(!is.null(smu1)){
    if(!is.null(x1)) warning("As 'smu1' is provided, 'x1' is not needed")
    if(!is.null(mu1)) warning("As 'smu1' is provided, 'mu1' is not needed")
  }
  if(!is.null(smu2)){
    if(!is.null(x2)) warning("As 'smu2' is provided, 'x2' is not needed")
    if(!is.null(mu2)) warning("As 'smu2' is provided, 'mu2' is not needed")
  }

  if (!is.null(x1)&!is.null(mu1)) {
    smu1 <- sqrt(sum((x1 - mu1)^2) / n1)
    DNAME_1 <- paste0(deparse(substitute(x1)), ", with sample size ", deparse(substitute(n1)))
  } else {
    DNAME_1 <- paste0(deparse(substitute(smu1)))
  }

  if (!is.null(x2)&!is.null(mu2)) {
    smu2 <- sqrt(sum((x2 - mu2)^2) / n2)
    DNAME_2 <- paste0(deparse(substitute(x2)), ", with sample size ", deparse(substitute(n2)))
  } else {
    DNAME_2 <- paste0(deparse(substitute(smu2)))
  }

  DNAME <- paste0(DNAME_1, ", and ", DNAME_2)

  NVAL <- 1

  # Statistic and pvalue
  STATISTIC <- smu1^2 / smu2^2
  if (alternative == "two.sided")
    PVALUE <- 2 * min(pf(STATISTIC, n1, n2), pf(STATISTIC, n1, n2, lower.tail = FALSE))
  else
    PVALUE <- pf(STATISTIC, n1, n2, lower.tail = (alternative == "less"))
  # Reject Region
  RR <- paste0("RR = ", switch(alternative,
                               two.sided = paste0("[0, ", round(qf(alpha/2, n1, n2), 5), "] U [",
                                                  round(qf(1 - alpha/2, n1, n2), 5), ", +\U221E)"),
                               greater = paste0("[", round(qf(1 - alpha, n1, n2), 5), ", +\U221E)"),
                               less = paste0("[0, ", round(qf(alpha, n1, n2), 5), "]")))

  # Plot
  if (plot) {
    ## Plot statistic distribution
    quantf <- qf(c(0.95,0.999), df1 = n1 - 1, df2 = n2 - 1)
    maxlimx <- max(ifelse(abs(diff(quantf))<10, quantf[2], quantf[1]), STATISTIC)
    minlimx <- min(qf(0.001, n1 - 1, n2 - 1), STATISTIC)
    curve(df(x, n1, n2), from = minlimx, to = maxlimx,
          main = bquote(T ~ "follows" ~ "F"[.(n1)][","][.(n2)]), axes = FALSE, xlab = "", ylab = "", lwd = lwd)
    u <- par("usr") # x0, x1, y0, y1
    rect(u[1], 0, u[2], u[4])
    xlimlen <- (u[2] - u[1])
    axis(2)
    legend("topright", c("p-value", "RR"), bty = "n", pch = c(22,NA), lty = c(NA,1), lwd = c(1,2),
           col = c("blue", "red"), pt.bg = adjustcolor('blue', alpha.f = 0.25), pt.cex = 2, seg.len = 1, cex = 1)
    if (alternative == "two.sided") {
      abline(h = 0, lwd = lwd + 1)
      lines(c(u[1], qf(alpha / 2, n1, n2)), c(0,0), col = "red", lwd = lwd + 1)
      lines(c(qf(1 - alpha / 2, n1, n2), u[2]), c(0,0), col = "red", lwd = lwd + 1)
      axis(1, pos = 0, col = NA, col.ticks = 1,
           at     = c(0, STATISTIC),
           labels = c(0, expression('T'[obs])))
      mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
      mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
      segments(x0 = c(qf(alpha / 2, n1, n2), qf(1 - alpha / 2, n1, n2)),
               y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
      segments(x0 = c(qf(alpha / 2, n1, n2), qf(1 - alpha / 2, n1, n2)),
               y0 = rep(c(-u[4]*0.015, u[4]*0.015), each = 2),
               x1 = c(qf(alpha / 2, n1, n2) - 0.01*xlimlen, qf(1 - alpha / 2, n1, n2) + 0.01*xlimlen),
               rep(c(-u[4]*0.015, u[4]*0.015), each = 2), col = "red", lwd = lwd + 1)
      if (abs(STATISTIC - qf(1 - alpha / 2, n1, n2)) > 0.05 * xlimlen &
          abs(STATISTIC - qf(alpha / 2, n1, n2)) > 0.05 * xlimlen) {
        axis(1, pos = 0, col = NA, col.ticks = NA,
             at     = c(qf(alpha / 2, n1, n2), qf(1 - alpha / 2, n1, n2)),
             labels = c(expression('F'[alpha/2]), expression('F'[1-alpha/2])))
        mtext("=", side = 1, line = 1.6, at = c(qf(alpha / 2, n1, n2), qf(1 - alpha / 2, n1, n2)), las = 2)
        mtext(round(qf(alpha / 2, n1, n2), 2), side = 1, line = 2.5, at = qf(alpha / 2, n1, n2))
        mtext(round(qf(1 - alpha / 2, n1, n2), 2), side = 1, line = 2.5, at = qf(1 - alpha / 2, n1, n2))
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
      # Statistic right tail
        segments(x0 = qf(PVALUE / 2, n1, n2, lower.tail = FALSE), y0 = 0,
                 x1 = qf(PVALUE / 2, n1, n2, lower.tail = FALSE),
                 y1 = df(qf(PVALUE / 2, n1, n2, lower.tail = FALSE), n1, n2),
                 col = 'blue', lwd = 1)
        x_vector <- seq(qf(PVALUE / 2, n1, n2, lower.tail = FALSE), maxlimx, length = 100)
        y_vector <- df(x_vector, n1, n2)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)

        # Statistic left tail
        segments(x0 = qf(PVALUE / 2, n1, n2), y0 = 0,
                 x1 = qf(PVALUE / 2, n1, n2), y1 = df(qf(PVALUE / 2, n1, n2), n1, n2),
                 col = 'blue', lwd = 1)
        x_vector <- seq(minlimx, qf(PVALUE / 2, n1, n2), length = 100)
        y_vector <- df(x_vector, n1, n2)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
      }

    } else {

      if (alternative == "less") {
        abline(h = 0, lwd = lwd + 1)
        lines(c(u[1], qf(alpha, n1, n2)), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at     = c(0, STATISTIC),
             labels = c(0, expression('T'[obs])))
        segments(x0 = qf(alpha, n1, n2), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qf(alpha, n1, n2), y0 = c(-u[4]*0.015, u[4]*0.015), x1 = qf(alpha, n1, n2) - 0.01*xlimlen,
                 c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qf(alpha, n1, n2)) > 0.05 * xlimlen) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qf(alpha, n1, n2), labels = expression('F'[alpha]))
          mtext("=", side = 1, line = 1.6, at = qf(alpha, n1, n2), las = 2)
          mtext(round(qf(alpha, n1, n2), 2), side = 1, line = 2.5, at = qf(alpha, n1, n2))
        }

      } else {
        abline(h = 0, lwd = lwd + 1)
        lines(c(qf(1 - alpha, n1, n2), u[2]), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at       = c(0, STATISTIC),
             labels   = c(0, expression('T'[obs])))
        segments(x0 = qf(1 - alpha, n1, n2), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qf(1 - alpha, n1, n2), y0 = c(-u[4]*0.015, u[4]*0.015), x1 = qf(1 - alpha, n1, n2) + 0.01*xlimlen,
                 c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qf(1 - alpha, n1, n2)) > 0.05 * xlimlen) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qf(1 - alpha, n1, n2), labels = expression('F'[1-alpha]))
          mtext("=", side = 1, line = 1.6, at = qf(1 - alpha, n1, n2), las = 2)
          mtext(round(qf(1 - alpha, n1, n2), 2), side = 1, line = 2.5, at = qf(1 - alpha, n1, n2))
        }
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic
        segments(x0 = STATISTIC, y0 = 0, x1 = STATISTIC, y1 = df(STATISTIC, n1, n2),
                 col = 'blue', lwd = 1)
        if (alternative == "less") {
          x_vector <- seq(minlimx, STATISTIC, length = 100)
        } else {
          x_vector <- seq(STATISTIC, maxlimx, length = 100)
        }
        y_vector <- df(x_vector, n1, n2)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
        mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
      }
    }
  }

  ##---------------------------------------------
  METHOD <- "Test for the ratio of population variances with known population means"
  DISTNAME <- "\U2208 F_{n\U2081,n\U2082}"
  STATFORMULA <- "S\U03BC\U2081\U00B2 / S\U03BC\U2082\U00B2"
  ESTIMATE <- setNames(smu1^2/smu2^2, "\U03C3\U2081\U00B2 / \U03C3\U2082\U00B2")
  PARAMETER <- c("num df" = n1, "denom df" = n2)
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

