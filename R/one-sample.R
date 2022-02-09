
#' One Sample Standard Error
#'
#' Calculates the students standard error
#' @param s the sample standard deviation
#' @param n the sample size
#' @return students se
#' @export
one_sample_se <- function(s, n) {
  return(s / sqrt(n))
}

#' One Sample Student's t-test
#'
#' @param mu the population mean
#' @param x the sample mean
#' @inheritParams one_sample_se
#' @inherit test_base
#' @seealso [stats::t.test()]
#' @seealso \code{t.test(data$X)}
#' @export
one_sample_t <- function(x, s, n, mu = 0, tail = 2, conf = 0.95) {
  df <- n - 1
  se <- one_sample_se(s, n)
  t <- (x - mu) / se
  p <- t_test(t, df, tail)
  moe <- t_crit(conf, df) * se
  ci <- interval(x, moe)

  stats <- setNames(data.frame(df, se, t, p), c("df", "se(x)", "t.stat", "p.value"))
  stats_print(
    method = "One Sample Student's t-test", tail = tail,
    stats = stats, conf = conf, ci = ci, moe = moe
  )
}

#' One Sample t-test Assumptions
#'
#' @inherit assumptions return description
#' @export
#'
#' @examples
#' one_sample_assumptions()
one_sample_assumptions <- function() {
  writeLines(c(
    "1. The data (variable) are continuous",
    "2. The population distribution of data is normally distributed",
    "3. The observations are independent"
  ))
}
