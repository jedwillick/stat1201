#' Not In
#'
#' Negation of \code{\%in\%}
#' @inheritParams base::match
#' @usage x \%ni\% table
#' @rdname ni
#' @export
#' @examples
#' 1 %ni% c(2, 3) # TRUE
#' "B" %ni% c("A", "B", "C") # FALSE
`%ni%` <- function(x, table) {
  return(!(x %in% table))
}

#' p value Strength
#'
#' Checks the strength of a p value.
#' @param p The p value
#' @return the strength of the p value
#' @export
p_evidence <- function(p) {
  if (p < 0.01) {
    return("strong")
  } else if (p < 0.05) {
    return("moderate")
  } else if (p < 0.1) {
    return("weak")
  } else {
    return("no")
  }
}

#' Z Critical
#'
#' Calculates the Z Critical value from a confidence.
#' @param conf The confidence as a decimal.
#' @return the Z Critical value
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' z_crit(0.95) # 95% Confident
#' z_crit(0.90) # 90% Confident
z_crit <- function(conf) {
  return(qnorm(1 - ((1 - conf) / 2)))
}

#' t Critical
#'
#' Calculates the t Critical value from a confidence.
#' @inheritParams z_crit
#' @param df the degrees of freedom
#' @return the t Critical value
#' @importFrom stats qt
#' @export
#' @examples
#' t_crit(0.95, 2) # 95% Confident
#' t_crit(0.90, 2) # 90% Confident
t_crit <- function(conf, df) {
  return(qt(1 - ((1 - conf) / 2), df))
}

#' Confidence Interval
#'
#' Calculates the confidence interval of a value.
#' @param x the value being intervaled
#' @param moe the margin of error
#' @export
#' @return \code{x +- moe}
interval <- function(x, moe) {
  return(c(x - moe, x + moe))
}

#' Outliers
#'
#' Displays the left and right bounds for outliers.
#' If an observation is provided displays whether that observation is an outlier.
#' @param Q1 the first quartile
#' @param Q3 the third quartile
#' @param obs the observation
outliers <- function(Q1, Q3, obs = NULL) {
  IQR <- Q3 - Q1
  left <- Q1 - (1.5 * IQR)
  right <- Q3 + (1.5 * IQR)
  if (is.null(obs)) {
    cat("Observation <", left, "\n")
    cat("Observation >", right, "\n")
  } else {
    if (obs < left) {
      sprintf("Outlier to the left: %g < %g", obs, left)
    } else if (obs > right) {
      sprintf("Outlier to the right: %g > %g", obs, right)
    } else {
      sprintf("%g is not an outlier!", obs)
    }
  }
}

#' t-test
#'
#' Performs a t-test.
#' @param t the t stat
#' @param df the degrees of freedom
#' @param tail specify either \code{1} or \code{2} tail
#' @return p value
#' @importFrom stats pt
#' @export
t_test <- function(t, df, tail) {
  if (tail %ni% c(1, 2)) stop("Tail must be 1 or 2.")
  return(tail * (1 - pt(abs(t), df)))
}

#' z-test
#'
#' Performs a z-test.
#' @param z the z stat
#' @param tail specify either \code{1} or \code{2} tail
#' @return p value
#' @importFrom stats pnorm
#' @export
z_test <- function(z, tail) {
  if (tail %ni% c(1, 2)) stop("Tail must be 1 or 2.")
  return(tail * (1 - pnorm(abs(z))))
}

#' Print Stats
#'
#' Prints the statistics in a nice format, and returns the values as list.
#' @param stats a data.frame with all the important stats.
#' @param method string describing the method used.
#' @param tail specify either \code{1} or \code{2} tail
#' @param conf the confidence
#' @param moe the margin of error
#' @param ci the confidence interval as \code{c(LL, UL)}
#' @param ... Any other objects to be included in the output.
#' @return the list of stats.
#' @export
stats_print <- function(stats, method, tail = NULL, conf = NULL, moe = NULL, ci = c(), ...) {
  x <- list(stats=stats, method=method, ...)
  rownames(x$stats) <- c("")
  x$stats <- rapply(object = x$stats, f = round, classes = "numeric", how = "replace", digits = 8)

  if (!is.null(x$stats$p.value)) x$stats$evidence <- p_evidence(x$stats$p.value)
  if (!is.null(tail)) {
    x$method <- sprintf("%d-Sided %s", tail, x$method)
    x$tail <- tail
  }

  cat("\n")
  cat(x$method, "\n\n")
  print(x$stats)
  cat("\n")

  if (!is.null(conf)) {
    cat(sprintf("%g%% Confidence", conf * 100), "\n")
    cat(sprintf("MOE: %g, CI: (%g, %g)", moe, ci[1], ci[2]))
    cat("\n")
    x$conf <- conf
    x$moe <- moe
    x$ci <- ci
  }

  invisible(c(x, x$stats))
}
