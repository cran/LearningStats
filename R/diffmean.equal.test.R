
diffmean.equal.test <- function(x, n=NULL, y, m=NULL, Scx=NULL, Scy=NULL, alternative,
                                alpha = 0.05, plot = TRUE, lwd = 1) {

  if(!is.null(Scx)){if(is.na(Scx)) stop("'sc1' must be a valid number")}
  if(!is.null(Scy)){if(is.na(Scy)) stop("'sc2' must be a valid number")}

  if (length(x) != 1L){bar_x <- mean(x);Scx=sd(x);n=length(x)}else{bar_x <- x}
  if (length(y) != 1L){bar_y <- mean(y);Scy=sd(y);m=length(y)}else{bar_y <- y}

  # check degrees of freedom
  if ((n + m - 2) < 1) stop("not enough observations")

  DNAME <- paste0(deparse(substitute(x)), ", with sample size ", deparse(substitute(n)), ", and ",
                  deparse(substitute(y)), ", with sample size ", deparse(substitute(m)))
  NVAL <- 0

  # Statistic and pvalue
  vart <- ((n - 1) * Scx^2 + (m - 1) * Scy^2) / (n + m - 2)
  STATISTIC <- (bar_x - bar_y) / (sqrt(vart * ((1 / n) + (1 / m))))
  if (alternative == "two.sided")
    PVALUE <- 2 * pt(abs(STATISTIC), df = n + m - 2, lower.tail = FALSE)
  else
    PVALUE <- pt(STATISTIC, df = n + m - 2, lower.tail = (alternative == "less"))
  # Reject Region
  RR <- paste0("RR = ", switch(alternative,
                               two.sided = paste0("(-\U221E, ", round(qt(alpha/2, n + m - 2), 5), "] U [",
                                                  round(qt(1 - alpha/2, n + m - 2), 5), ", +\U221E)"),
                               greater = paste0("[", round(qt(1 - alpha, n + m - 2), 5), ", +\U221E)"),
                               less = paste0("(-\U221E, ", round(qt(alpha, n + m - 2), 5), "]")))

  # Plot
  if (plot) {
    ## Plot statistic distribution
    curve(dt(x, df = n + m - 2), from = min(-3, -abs(STATISTIC) - 0.1), to = max(3, abs(STATISTIC) + 0.1),
          main = bquote(T ~ "follows" ~ "T"[.(n + m - 2)]), axes = FALSE, xlab = "", ylab = "", lwd = lwd)
    u <- par("usr") # x0, x1, y0, y1
    rect(u[1], 0, u[2], u[4])
    axis(2)
    legend("topright", c("p-value", "RR"), bty = "n", pch = c(22,NA), lty = c(NA,1), lwd = c(1,2),
           col = c("blue", "red"), pt.bg = adjustcolor('blue', alpha.f = 0.25), pt.cex = 2, seg.len = 1, cex = 1)
    if (alternative == "two.sided") {
      abline(h = 0, lwd = lwd + 1)
      lines(c(u[1], qt(alpha / 2, n + m - 2)), c(0,0), col = "red", lwd = lwd + 1)
      lines(c(qt(1 - alpha / 2, n + m - 2), u[2]), c(0,0), col = "red", lwd = lwd + 1)
      axis(1, pos = 0, col = NA, col.ticks = 1,
           at     = c(0, STATISTIC, -1*STATISTIC),
           labels = c(0, expression('T'[obs]), expression(-'T'[obs])))
      segments(x0 = c(qt(alpha / 2, n + m - 2), qt(1 - alpha / 2, n + m - 2)),
               y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
      segments(x0 = c(qt(alpha / 2, n + m - 2), qt(1 - alpha / 2, n + m - 2)),
               y0 = rep(c(-u[4]*0.015, u[4]*0.015), each = 2),
               x1 = c(qt(alpha / 2, n + m - 2) - u[2]*0.015, qt(1 - alpha / 2, n + m - 2) + u[2]*0.015),
               rep(c(-u[4]*0.015, u[4]*0.015), each = 2), col = "red", lwd = lwd + 1)
      if ((abs(STATISTIC) - qt(1 - alpha / 2, n + m - 2)) > 0.3) {
        axis(1, pos = 0, col = NA, col.ticks = NA,
             at     = c(qt(alpha / 2, n + m - 2), qt(1 - alpha / 2, n + m - 2)),
             labels = c(expression(-'t'[1-alpha/2]), expression('t'[1-alpha/2])))
        mtext("=", side = 1, line = 1.6, at = c(qt(alpha / 2, n + m - 2), qt(1 - alpha / 2, n + m - 2)), las = 2)
        mtext(round(qt(alpha / 2, n + m - 2), 2), side = 1, line = 2.5, at = qt(alpha / 2, n + m - 2))
        mtext(round(qt(1 - alpha / 2, n + m - 2), 2), side = 1, line = 2.5, at = qt(1 - alpha / 2, n + m - 2))
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic right tail
        segments(x0 = qt(1 - PVALUE / 2, n + m - 2), y0 = 0,
                 x1 = qt(1 - PVALUE / 2, n + m - 2), y1 = dt(qt(1 - PVALUE / 2, n + m - 2), n + m - 2),
                 col = 'blue', lwd = 1)
        x_vector <- seq(qt(1 - PVALUE / 2, n + m - 2), 4, length = 100)
        y_vector <- dt(x_vector, n + m - 2)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = qt(1 - PVALUE / 2, n + m - 2), las = 2)
        mtext(round(qt(1 - PVALUE / 2, n + m - 2), 2), side = 1, line = 2.5, at = qt(1 - PVALUE / 2, n + m - 2))

        # Statistic left tail
        segments(x0 = qt(PVALUE / 2, n + m - 2), y0 = 0,
                 x1 = qt(PVALUE / 2, n + m - 2), y1 = dt(qt(PVALUE / 2, n + m - 2), n + m - 2),
                 col = 'blue', lwd = 1)
        x_vector <- seq(-4, qt(PVALUE / 2, n + m - 2), length = 100)
        y_vector <- dt(x_vector, n + m - 2)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = qt(PVALUE / 2, n + m - 2), las = 2)
        mtext(round(qt(PVALUE / 2, n + m - 2), 2), side = 1, line = 2.5, at = qt(PVALUE / 2, n + m - 2))
      }

    } else {

      if (alternative == "less") {
        abline(h = 0, lwd = lwd + 1)
        lines(c(u[1], qt(alpha, n + m - 2)), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at     = c(0, STATISTIC),
             labels = c(0, expression('T'[obs])))
        segments(x0 = qt(alpha, n + m - 2), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qt(alpha, n + m - 2), y0 = c(-u[4]*0.015, u[4]*0.015),
                 x1 = qt(alpha, n + m - 2) - u[2]*0.015, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qt(alpha, n + m - 2)) > 0.3) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qt(alpha, n + m - 2), labels = expression(-'t'[1-alpha]))
          mtext("=", side = 1, line = 1.6, at = qt(alpha, n + m - 2), las = 2)
          mtext(round(qt(alpha, n + m - 2), 2), side = 1, line = 2.5, at = qt(alpha, n + m - 2))
        }

      } else {
        abline(h = 0, lwd = lwd + 1)
        lines(c(qt(1 - alpha, n + m - 2), u[2]), c(0,0), col = "red", lwd = lwd + 1)
        axis(1, pos = 0, col = NA, col.ticks = 1,
             at       = c(0, STATISTIC),
             labels   = c(0, expression('T'[obs])))
        segments(x0 = qt(1 - alpha, n + m - 2), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
        segments(x0 = qt(1 - alpha, n + m - 2), y0 = c(-u[4]*0.015, u[4]*0.015),
                 x1 = qt(1 - alpha, n + m - 2) + u[2]*0.015, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
        if (abs(STATISTIC - qt(1 - alpha, n + m - 2)) > 0.3) {
          axis(1, pos = 0, col = NA, col.ticks = NA, at = qt(1 - alpha, n + m - 2), labels = expression('t'[1-alpha]))
          mtext("=", side = 1, line = 1.6, at = qt(1 - alpha, n + m - 2), las = 2)
          mtext(round(qt(1 - alpha, n + m - 2), 2), side = 1, line = 2.5, at = qt(1 - alpha, n + m - 2))
        }
      }

      # pvalue != 0
      if (PVALUE > .Machine$double.eps) {
        # Statistic
        segments(x0 = STATISTIC, y0 = 0, x1 = STATISTIC, y1 = dt(STATISTIC, n + m - 2),
                 col = 'blue', lwd = 1)
        if (alternative == "less") {
          x_vector <- seq(-4, STATISTIC, length = 100)
        } else {
          x_vector <- seq(STATISTIC, 4, length = 100)
        }
        y_vector <- dt(x_vector, n + m - 2)
        polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
                col = adjustcolor('blue', alpha.f = 0.25), border = NA)
        mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
        mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
      }
    }
  }

  ##---------------------------------------------
  METHOD <- "Test for the difference between the means of two Normal populations with the same unknown variances"
  DISTNAME <- "\U2208 T_{n\U2081 + n\U2082 - 2}"
  STATFORMULA <- "(bar.x\U2081 - bar.x\U2082) / (Sw sqrt((1 / n\U2081) +
                  (1 / n\U2082))), \t Sw\U00B2 = ((n\U2081 - 1) Sc\U2081\U00B2 +
                  (n\U2082 - 1) Sc\U2082\U00B2) / (n\U2081 + n\U2082 - 2)"
  ESTIMATE <- setNames(bar_x - bar_y, "\U03BC\U2081 - \U03BC\U2082")
  PARAMETER <- n + m - 2
  names(NVAL) <- names(ESTIMATE)
  names(PARAMETER) <- "df"
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

