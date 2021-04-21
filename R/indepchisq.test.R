
#' Chi-squared Independence Test for Categorical Data.
#'
#' \code{indepchisq.test} allows to computes Chi-squared independence hypothesis test for two categorical values.
#'
#' @param Oij observed frequencies. A numeric matrix, a table or a data.frame with the
#' observed frequencies can be passed. If missing, arguments \code{x} and \code{y} must be supplied.
#' @param x a vector (numeric or character) or factor with the first categorical variable.
#' @param y a vector (numeric or character) or factor with the second categorical variable. It should be of the same
#' length as \code{x}.
#' @param alpha a single number in (0,1), significance level.
#' @param plot a logical indicating whether to plot the rejection region and p-value.
#' @param lwd a single number indicating the line width of the plot.
#'
#' @details The expected frequencies are calculated as follows
#' \deqn{E_{ij}=\frac{n_{i\bullet}\times n_{\bullet j}}{n},} and the test statistic
#' is given by \deqn{T = \sum_{i,j} \frac{(n_{ij} - E_{ij})^2}{E_{ij}},}
#' \eqn{T \in \chi^2_{(r-1)(s-1)}}, where \eqn{n} is the number of observations, \eqn{n_{i\bullet}}
#'  is the marginal frequency of category i of variable x, \eqn{n_{\bullet j}}
#' is the marginal frequency of category j of variable y, r is the number of categories in
#' variable x and s the number of categories in variable y.
#'
#' The null hypothesis is rejected when \eqn{T > \chi^2_{(r-1)(s-1),1-\alpha}}, where \eqn{\chi^2_{(r-1)(s-1),1-\alpha}}
#' is the \eqn{1-\alpha} quantile of a \eqn{\chi^2} distribution with \eqn{(r-1)(s-1)} degrees of freedom.
#'
#' @return A list with class "\code{lstest}" and "\code{htest}" containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degrees of freedom of the statistic's distribution.}
#' \item{p.value}{the p-value of the test.}
#' \item{estimate}{a numeric matrix with the estimated frequencies Eij.}
#' \item{method}{a character string indicating the method used.}
#' \item{data.name}{a character string giving the names of the data.}
#' \item{alpha}{the significance level.}
#' \item{dist.name}{a character string indicating the distribution of the test statistic.}
#' \item{statformula}{a character string with the statistic's formula.}
#' \item{reject.region}{a character string with the reject region.}
#' \item{obs.freq}{a numeric matrix with the observed frequencies Oij.}
#'
#' @examples
#' Oij <- matrix(c( 20,    8,
#'                 934, 1070,
#'                 113,   92), ncol = 2, byrow = TRUE)
#' indepchisq.test(Oij)
#' @export
indepchisq.test <- function(Oij, x, y, alpha = 0.05, plot = TRUE, lwd = 1) {

  if ( sum(c(missing(x), missing(y))) == 1 ) stop("Both 'x' and 'y' must be specified, otherwise 'Oij' should be passed")
  if (!missing(x)) {
    if(!is.vector(x) & !is.factor(x))stop("'x' must a numeric vector or factor")
    if(!is.vector(y) & !is.factor(y))stop("'y' must a numeric vector or factor")
    if (length(x) != length(y)) stop("'x' and 'y' must have the same length")
    OK  <- complete.cases(x, y)
    x   <- factor(x[OK])
    y   <- factor(y[OK])
    Oij <- table(x, y)
  }
  if(!is.matrix(Oij) & !is.data.frame(Oij) & !is.table(Oij)) stop("'Oij' must be a numeric matrix, a table or a data.frame")
  if (class(Oij)[1] == "table") Oij <- unclass(Oij)
  if (is.data.frame(Oij)) Oij <- as.matrix(Oij)
  ok <- complete.cases(Oij)
  if (sum(!ok) != 0) stop("Missing values across 'Oij'")
  if (any(Oij < 0)) stop("Elements of 'Oij' must be nonnegative")
  if (any(dim(Oij) < 2)) stop("Incorrect dimension of matrix 'Oij'")

  if ( !((length(alpha) == 1L) && is.finite(alpha) && (alpha > 0) && (alpha < 1)) )
    stop("'alpha' must be a single number in (0,1)")

  DNAME <- paste0(deparse(substitute(Oij)))

  if (is.null(rownames(Oij))) rownames(Oij) <- paste0("cx", 1:nrow(Oij))
  if (is.null(colnames(Oij))) colnames(Oij) <- paste0("cy", 1:ncol(Oij))

  if(!is.numeric(lwd)|length(lwd)!=1) stop("The argument 'lwd' must be a positive integer")
  if(!is.finite(lwd)|lwd<=0|lwd!=round(lwd,0)) stop("The argument 'lwd' must be a positive integer")
  if(length(plot)!=1){stop("'plot' must be a single logical value")}
  if(!is.logical(plot)|is.na(plot)) stop("'plot' must be a single logical value")

  # Statistic and pvalue
  N      <- sum(Oij)
  ft     <- Oij / N
  margx  <- apply(ft, 1, sum)
  margy  <- apply(ft, 2, sum)
  Eij    <- outer(margx, margy) * N
  df_chi <- (length(margx) - 1) * (length(margy) - 1)

  if (any(Eij < 5)) warning("Chi-squared approximation might not be accurate due to expected values less than 5")

  STATISTIC <- sum((Eij - Oij)^2 / Eij)
  PVALUE <- pchisq(STATISTIC, df_chi, lower.tail = FALSE)
  # Reject Region
  RR <- paste0("RR = ", paste0("[", round(qchisq(1 - alpha, df_chi), 5), ", +\U221E)"))

  # Plot
  if (plot) {
    ## Plot statistic distribution
    maxlimx <- max(1.2*(2*(df_chi)), STATISTIC + 1)
    curve(dchisq(x, df = df_chi), from = 0, to = maxlimx,
          main = bquote(chi^"2" ~ "follows" ~ chi[.(df_chi)]^"2"), axes = FALSE, xlab = "", ylab = "", lwd = lwd)
    u <- par("usr") # x0, x1, y0, y1
    rect(u[1], ifelse(u[3]<0, 0, u[3]), u[2], u[4])
    axis(2)
    legend("topright", c("p-value", "RR"), bty = "n", pch = c(22,NA), lty = c(NA,1), lwd = c(1,2),
           col = c("blue", "red"), pt.bg = adjustcolor('blue', alpha.f = 0.25), pt.cex = 2, seg.len = 1, cex = 1)

    abline(h = ifelse(u[3]<0, 0, u[3]), lwd = lwd + 1)
    lines(c(qchisq(1 - alpha, df_chi), u[2]), rep(ifelse(u[3]<0, 0, u[3]),2), col = "red", lwd = lwd + 1)
    axis(1, pos = ifelse(u[3]<0, 0, u[3]), col = NA, col.ticks = 1,
         at       = c(0, STATISTIC),
         labels   = c(0, expression(chi[obs]^'2')))
    segments(x0 = qchisq(1 - alpha, df_chi), y0 = -u[4]*0.015, y1 = u[4]*0.015, col = "red", lwd = lwd + 1)
    segments(x0 = qchisq(1 - alpha, df_chi), y0 = c(-u[4]*0.015, u[4]*0.015),
             x1 = qchisq(1 - alpha, df_chi) + 0.01*maxlimx, c(-u[4]*0.015, u[4]*0.015), col = "red", lwd = lwd + 1)
    if (abs(STATISTIC - qchisq(1 - alpha, df_chi)) > 0.05 * maxlimx) {
      axis(1, pos = ifelse(u[3]<0, 0, u[3]), col = NA, col.ticks = NA, at = qchisq(1 - alpha, df_chi),
           labels = expression(chi[1-alpha]^'2'))
      mtext("=", side = 1, line = 1.6, at = qchisq(1 - alpha, df_chi), las = 2)
      mtext(round(qchisq(1 - alpha, df_chi), 2), side = 1, line = 2.5, at = qchisq(1 - alpha, df_chi))
    }

      # Statistic
      segments(x0 = qchisq(PVALUE, df_chi, lower.tail = FALSE), y0 = 0,
               x1 = qchisq(PVALUE, df_chi, lower.tail = FALSE), y1 = dchisq(qchisq(PVALUE, df_chi, lower.tail = FALSE), df_chi),
               col = 'blue', lwd = 1)
      x_vector <- seq(STATISTIC, maxlimx, length = 100)
      y_vector <- dchisq(x_vector, df_chi)
      polygon(c(x_vector, rev(x_vector)), c(y_vector, rep(0, length(y_vector))),
              col = adjustcolor('blue', alpha.f = 0.25), border = NA)
      mtext("=", side = 1, line = 1.6, at = STATISTIC, las = 2)
      mtext(round(STATISTIC, 2), side = 1, line = 2.5, at = STATISTIC)
  }

  ##---------------------------------------------
  METHOD <- "Chi-squared test for independence"
  DISTNAME <- "\U2208 X\U00B2_{(r-1)(s-1)}"
  STATFORMULA <- "\U2211\U1d62\U2c7c (n\U1d62\U2c7c - E\U1d62\U2c7c)\U00B2 / E\U1d62\U2c7c"
  ESTIMATE <- Eij
  rownames(ESTIMATE) <- rownames(Oij)
  colnames(ESTIMATE) <- colnames(Oij)
  PARAMETER <- df_chi
  names(PARAMETER) <- "df"
  names(STATISTIC) <- "X\U00B2"
  RVAL <- list(
    statistic = STATISTIC,
    parameter = PARAMETER,
    p.value = as.numeric(PVALUE),
    estimate = ESTIMATE,
    method = METHOD,
    data.name = DNAME,
    alpha = alpha,
    dist.name = DISTNAME,
    statformula = STATFORMULA,
    reject.region = RR,
    obs.freq = Oij
  )
  class(RVAL) <- c("lstest", "htest")
  return(RVAL)
}

