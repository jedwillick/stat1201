#' Correlation Between Two Variables DF
#'
#' @param n sample size
#'
#' @return Degrees of Freedom
#' @export
#'
#' @examples
#' correlation_df(20)
correlation_df <- function(n) {
  return(n - 2)
}

#' Correlation Standard Error
#'
#' @param r the sample correlation coefficient can be found with `cor(x, y)`
#' @param n the sample size
#'
#' @return The correlation standard error
#' @seealso [stats::cor()]
#' @seealso [stats::cor.test()]
#' @export
correlation_se <- function(r, n) {
  return(sqrt((1 - (r^2)) / (n - 2)))
}


#' Correlation t-test
#'
#' Uses Fisher's Z Transformation for Confidence Interval
#' @inherit  test_base
#' @inherit correlation_se
#' @param p the population correlation coefficient
#' @export
#'
#' @examples
#' correlation_t(0.6642512, 20, 2)
correlation_t <- function(r, n, tail = 2, p = 0, conf = 0.95) {
  df <- n - 2
  se <- correlation_se(r, n)
  t <- (r - p) / se
  p <- t_test(t, df, tail)

  # Fisher's Z Transformation
  moe <- z_crit(conf) * sqrt(1 / (n - 3))
  ci <- tanh(interval(atanh(r), moe))

  stats <- setNames(data.frame(df, se, t, p), c("df", "se(r)", "t.stat", "p.value"))
  stats_print(
    method = "Correlation t-test", tail = tail,
    stats = stats, conf = conf, ci = ci, moe = moe
  )
}
