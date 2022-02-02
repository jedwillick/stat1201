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
#' @inherit  test_base
#' @inherit correlation_se
#' @param p the population correlation coefficient
#' @export
correlation_t <- function(r, n, tail, p = 0, conf = 0.95) {
  df <- n - 2
  se <- correlation_se(r, n)
  t <- (r - p) / se
  p <- t_test(t, df, tail)
  # TODO: Ask about CI not giving right result
  moe <- t_crit(conf, df) * se
  ci <- interval(r, moe)

  stats <- setNames(data.frame(df, se, t, p), c("df", "se(r)", "t.stat", "p.value"))
  stats_print(
    method = "Correlation t-test", tail = tail,
    stats = stats, conf = conf, ci = ci, moe = moe
  )
}
