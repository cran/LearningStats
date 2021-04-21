diffvariance.unknown.test <- function(x1 = NULL, x2 = NULL, sc1 = NULL, sc2 = NULL, n1 = NULL, n2 = NULL,
                                      alternative = alternative, alpha = 0.05, plot = TRUE, lwd = 1) {
  if(!is.null(x1)){
#    if(!is.null(sc1)){warning("As sample1 is provided, 'sc1' is omitted and computed from the sample")}
    n1=length(x1);sc1=sd(x1)
  }
  if(!is.null(x2)){
#    if(!is.null(sc2)){warning("As sample2 is provided, 'sc2' is omitted and computed from the sample")}
    n2=length(x2);sc2=sd(x2)
  }


  if (n1 <= 2L) stop("To have enough observations 'n1' must be an integer bigger than 2")
  if (n2 <= 2L) stop("To have enough observations 'n2' must be an integer bigger than 2")

  if (!is.null(x1)) Sx2 <- var(x1)
    else Sx2 <- sc1^2

  if (!is.null(x2)) Sy2 <- var(x2)
    else Sy2 <- sc2^2

  DNAME <- paste0(Sx2, ", with sample size ", deparse(substitute(n1)), ", and ",
                  Sy2, ", with sample size ", deparse(substitute(n2)))
  NVAL <- 1

  # Statistic and pvalue
  STATISTIC <- Sx2 / Sy2
  if (alternative == "two.sided")
    PVALUE <- 2 * min(pf(STATISTIC, n1 - 1, n2 - 1), pf(STATISTIC, n1 - 1, n2 - 1, lower.tail = FALSE))
  else
    PVALUE <- pf(STATISTIC, n1 - 1, n2 - 1, lower.tail = (alternative == "less"))
  # Reject Region
  RR <- paste0("RR = ", switch(alternative,
                               two.sided = paste0("[0, ", round(qf(alpha/2, n1 - 1, n2 - 1), 5), "] U [",
                                                  round(qf(1 - alpha/2, n1 - 1, n2 - 1), 5), ", +\U221E)"),
                               greater = paste0("[", round(qf(1 - alpha, n1 - 1, n2 - 1), 5), ", +\U221E)"),
                               less = paste0("[0, ", round(qf(alpha, n1 - 1, n2 - 1), 5), "]")))

  # Plot
  if (plot) {
    ## Plot statistic distribution
    quantf <- qf(c(0.95,0.999), df1 = n1 - 1, df2 = n2 - 1)
    maxlimx <- max(ifelse(abs(diff(quantf))<10, quantf[2], quantf[1]), STATISTIC)
    minlimx <- min(qf(0.001, n1 - 1, n2 - 1), STATISTIC)
    curve(df(x, n1 - 1, n2 - 1), from = minlimx, to = maxlimx,
          main = bquote(T ~ "follows" ~ "F"[.(n1-1)][","][.(n2-1)]), axes = FALSE, xlab = "", ylab = "", lwd = lwd)
    u <- par("usr") # x0, x1, y0, y1
    rect(u[1], 0, u[2], u[4])
    xlimlen <- (u[2] - u[1])
    axis(2)
    legend("topright", c("p-value", "RR"), bty = "n", pch = c(22,NA), lty = c(NA,1), lwd = c(1,2),
           col = c("blue", "red"), pt.bg = adjustcolor('blue', alpha.f = 0.25), pt.cex = 2, seg.len = 1, cex = 1)
    if (alternative == "two.sided") {
      abline(h = 0, lwd = lwd + 1)
      lines(c(u[1], qf(alpha / 2, n1 - 1, n2 - 1)), c(0,0), col = "red", lwd = lwd + 1)
      lines(c(qf(1 - alpha / 2, n1 - 1, n2 - 1), u[2]), c(0,0), col = "red", lwd = lwd + 1)
      axis(1, pos = 0, col = NA, col.ticks = 1,
           at     = c(0, STATISTIC),
           labels = c(0, expression('T'[obs])))
      mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
      mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
      segments(x0 = c(qf(alpha / 2, n1 - 1, n2 - 1), qf(1 - alpha / 2, n1 - 1, n2 - 1)),
               y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
      segments(x0 = c(qf(alpha / 2, n1 - 1, n2 - 1), qf(1 - alpha / 2, n1 - 1, n2 - 1)),
               y0 = rep(c(-u[4]*0.015, u[4]*0.015), each = 2),
               x1 = c(qf(alpha / 2, n1 - 1, n2 - 1) - 0.01*xlimlen, qf(1 - alpha / 2, n1 - 1, n2 - 1) + 0.01*xlimlen),
               rep(c(-u[4]*0.015, u[4]*0.015), each = 2), col = "red", lwd = lwd + 1)
      if (abs(STATISTIC - qf(1 - alpha / 2, n1 - 1, n2 - 1)) > 0.05 * xlimlen &
          abs(STATISTIC - qf(alpha / 2, n1 - 1, n2 - 1)) > 0.05 * xlimlen) {
        axis(1, pos = 0, col = NA, col.ticks = NA,
             at     = c(qf(alpha / 2, n1 - 1, n2 - 1), qf(1 - alpha / 2, n1 - 1, n2 - 1)),
             labels = c(expression('F'[alpha/2]), expression('F'[1-alpha/2])))
        mtext("=", side = 1, line = 1.6, at = c(qf(alpha / 2, n1 - 1, n2 - 1), qf(1 - alpha / 2, n1 - 1, n2 - 1)), las = 2)
        mtext(round(qf(alpha / 2, n1 - 1, n2 - 1), 2), side = 1, line = 2.5, at = qf(alpha / 2, n1 - 1, n2 - 1))
        mtext(round(qf(1 - alpha / 2, n1 - 1, n2 - 1), 2), side = 1, line = 2.5, at = qf(1 - alpha / 2, n1 - 1, n2 - 1))
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic right tail
        segments(x0 = qf(PVALUE / 2, n1 - 1, n2 - 1, lower.tail = FALSE), y0 = 0,
                 x1 = qf(PVALUE / 2, n1 - 1, n2 - 1, lower.tail = FALSE),
                 y1 = df(qf(PVALUE / 2, n1 - 1, n2 - 1, lower.tail = FALSE), n1 - 1, n2 - 1),
                 col = 'blue', lwd = 1)
        x_vector <- seq(qf(PVALUE / 2, n1 - 1, n2 - 1, lower.tail = FALSE), maxlimx, length = 100)
        y_vector <- df(x_vector, n1 - 1, n2 - 1)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)

        # Statistic left tail
        segments(x0 = qf(PVALUE / 2, n1 - 1, n2 - 1), y0 = 0,
                 x1 = qf(PVALUE / 2, n1 - 1, n2 - 1), y1 = df(qf(PVALUE / 2, n1 - 1, n2 - 1), n1 - 1, n2 - 1),
                 col = 'blue', lwd = 1)
        x_vector <- seq(minlimx, qf(PVALUE / 2, n1 - 1, n2 - 1), length = 100)
        y_vector <- df(x_vector, n1 - 1, n2 - 1)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
      }

    } else {

      if (alternative == "less") {
        abline(h = 0, lwd = lwd + 1)
        lines(c(u[1], qf(alpha, n1 - 1, n2 - 1)), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at     = c(0, STATISTIC),
             labels = c(0, expression('T'[obs])))
        segments(x0 = qf(alpha, n1 - 1, n2 - 1), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qf(alpha, n1 - 1, n2 - 1), y0 = c(-u[4]*0.015, u[4]*0.015),
                 x1 = qf(alpha, n1 - 1, n2 - 1) - 0.01*xlimlen, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qf(alpha, n1 - 1, n2 - 1)) > 0.05 * xlimlen) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qf(alpha, n1 - 1, n2 - 1), labels = expression('F'[alpha]))
          mtext("=", side = 1, line = 1.6, at = qf(alpha, n1 - 1, n2 - 1), las = 2)
          mtext(round(qf(alpha, n1 - 1, n2 - 1), 2), side = 1, line = 2.5, at = qf(alpha, n1 - 1, n2 - 1))
        }

      } else {
        abline(h = 0, lwd = lwd + 1)
        lines(c(qf(1 - alpha, n1 - 1, n2 - 1), u[2]), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at       = c(0, STATISTIC),
             labels   = c(0, expression('T'[obs])))
        segments(x0 = qf(1 - alpha, n1 - 1, n2 - 1), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qf(1 - alpha, n1 - 1, n2 - 1), y0 = c(-u[4]*0.015, u[4]*0.015),
                 x1 = qf(1 - alpha, n1 - 1, n2 - 1) + 0.01*xlimlen, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qf(1 - alpha, n1 - 1, n2 - 1)) > 0.05 * xlimlen) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qf(1 - alpha, n1 - 1, n2 - 1), labels = expression('F'[1-alpha]))
          mtext("=", side = 1, line = 1.6, at = qf(1 - alpha, n1 - 1, n2 - 1), las = 2)
          mtext(round(qf(1 - alpha, n1 - 1, n2 - 1), 2), side = 1, line = 2.5, at = qf(1 - alpha, n1 - 1, n2 - 1))
        }
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic
        segments(x0 = STATISTIC, y0 = 0, x1 = STATISTIC, y1 = df(STATISTIC, n1 - 1, n2 - 1),
                 col = 'blue', lwd = 1)
        if (alternative == "less") {
          x_vector <- seq(minlimx, STATISTIC, length = 100)
        } else {
          x_vector <- seq(STATISTIC, maxlimx, length = 100)
        }
        y_vector <- df(x_vector, n1 - 1, n2 - 1)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
        mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
      }
    }
  }

  ##---------------------------------------------
  METHOD <- "Test for the ratio of population variances with unknown population means"
  DISTNAME <- "\U2208 F_{n\U2081-1,n\U2082-1}"
  STATFORMULA <- "Sc\U2081\U00B2 / Sc\U2082\U00B2"
  ESTIMATE <- setNames(Sx2/Sy2, "\U03C3\U2081\U00B2 / \U03C3\U2082\U00B2")
  PARAMETER <- c("num df" = n1 - 1, "denom df" = n2 - 1)
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

