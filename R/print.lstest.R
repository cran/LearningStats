#' @export
print.lstest <- function(x, ...) {
  cat("\n")
  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  if (is.null(x$alternative)) {
    cat("\U0048\U2080: X and Y are independent", "\n")
    cat("\U0048\U2090: X and Y are not independent", "\n")
  } else {
    cat(paste0("\U0048\U2080: ", names(x$estimate),
               switch(x$alternative, two.sided = " = ", greater = " \U2264 ", less = " \U2265 "),
               x$null.value), x$unit, "\n")
    cat(paste0("\U0048\U2090: ", names(x$estimate),
               switch(x$alternative, two.sided = " \U2260 ", greater = " > ", less = " < "),
               x$null.value), x$unit, "\n")
  }
  cat(paste0(names(x$statistic), " = ", x$statformula), "\n")
  cat(paste0(names(x$statistic), " ", x$dist.name), "\n")
  cat(paste0("\U03b1 = ", x$alpha), "\n")
  cat(paste0(names(x$statistic), "_obs = ", round(x$statistic, 5)), "\n")
  cat(x$reject.region, "\n")
  cat(paste0("p-value = ", round(x$p.value, 5)), "\n")
  if (!is.null(x$obs.freq)) {
    cat("Observed frequencies:", "\n")
    print(round(x$obs.freq, 2))
    cat("Expected frequencies:", "\n")
    print(round(x$estimate, 2))
  }
}
