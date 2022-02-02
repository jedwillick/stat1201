
#' Student's Standard Error
#'
#' Calculates the students standard error
#' @param s the sample standard deviation
#' @param n the sample size
#' @return students se
#' @export
students_se <- function(s, n) {
  return(s / sqrt(n))
}

#' Student's One Sample t-test
#'
#' @param mu the population mean
#' @param x the sample mean
#' @inheritParams students_se
#' @inherit test_base
#' @seealso \code{stats::t.test(data$X)}
#' @export
students_t <- function(mu, x, s, n, tail, conf = 0.95) {
  df <- n - 1
  se <- students_se(s, n)
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
