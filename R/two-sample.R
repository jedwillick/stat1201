#' Two Sample t-test Assumptions
#'
#' @inherit assumptions return description
#' @export
#'
#' @examples
#' two_sample_assumptions()
two_sample_assumptions <- function() {
  writeLines(c(
    "1. The data follows a normal distribution in each population.",
    "2. The two samples are independent.",
    "3. Each observation is a random sample from their respective populations."
  ))
}

#' Two Sample Standard Error
#'
#' @param s1 1st sample SD
#' @param n1 1st sample size
#' @param s2 2nd sample SD
#' @param n2 2nd sample size
#'
#' @return the two sample se(x1 - x2)
#' @export
two_sample_se <- function(s1, n1, s2, n2) {
  return(sqrt(((s1^2) / n1) + ((s2^2) / n2)))
}

#' Two Sample t-test SD's Not Equal
#'
#' @param x1 1st sample mean
#' @param x2 2nd sample mean
#' @inheritParams two_sample_se
#' @inherit test_base
#' @seealso [stats::t.test()]
#' @seealso \code{t.test(Y ~ X, data)}
#' @export
two_sample_t <- function(x1, s1, n1, x2, s2, n2, tail, conf = 0.95) {
  df <- min(c(n1 - 1, n2 - 1))
  se <- two_sample_se(s1, n1, s2, n2)
  t <- ((x1 - x2) - 0) / (se)
  p <- t_test(t, df, tail)
  moe <- t_crit(conf, df) * se
  ci <- interval(abs(x1 - x2), moe)

  stats <- setNames(data.frame(df, se, t, p), c("df", "se(x1-x2)", "t.stat", "p.value"))
  stats_print(
    method = "Two Sample t-test SD's Not Equal", tail = tail,
    stats = stats, conf = conf, ci = ci, moe = moe
  )
}

#' Pooled Variance
#'
#' @inheritParams two_sample_se
#' @return Pooled variance
#' @export
pooled_S2p <- function(s1, n1, s2, n2) {
  numerator <- ((n1 - 1) * (s1^2)) + ((n2 - 1) * (s2^2))
  denominator <- (n1 - 1) + (n2 - 1)
  return(numerator / denominator)
}

#' Pooled Standard Error
#'
#' @inheritParams two_sample_se
#' @return Pooled se(x1 - x2)
#' @export
pooled_se <- function(s1, n1, s2, n2) {
  S2p <- pooled_S2p(s1, n1, s2, n2)
  return(sqrt(S2p * ((1 / n1) + (1 / n2))))
}

#' Pooled t-test SD's Equal
#'
#' @inherit two_sample_t
#' @seealso [stats::t.test()]
#' @seealso \code{t.test(Y ~ X, data, var.equal = TRUE)}
#' @export
pooled_t <- function(x1, s1, n1, x2, s2, n2, tail, conf = 0.95) {
  df <- n1 + n2 - 2
  S2p <- pooled_S2p(s1, n1, s2, n2)
  se <- pooled_se(s1, n1, s2, n2)
  t <- ((x1 - x2) - 0) / (se)
  p <- t_test(t, df, tail)
  moe <- t_crit(conf, df) * se
  ci <- interval(abs(x1 - x2), moe)

  stats <- setNames(data.frame(df, S2p, se, t, p), c("df", "S2p", "se(x1-x2)", "t.stat", "p.value"))
  stats_print(
    method = "Pooled t-test SD's Equal", tail = tail,
    stats = stats, conf = conf, ci = ci, moe = moe
  )
}

#' Two Proportions t-test Assumptions
#'
#' @inherit assumptions return description
#' @export
#' @examples
#' two_prop_assumptions()
two_prop_assumptions <- function() {
  writeLines(c(
    "1. Each observation in the sample is randomly selected from their respective populations.",
    "2. Each population is independent.",
    "3. Populations follow binomial distributions.",
    "4. Both np and n(1-p) are greater than 5 in order to use the normal approximation for binomial distributions."
  ))
}

#' Two Proportions Standard Error
#'
#' @param phat1 1st sample proportion
#' @param phat2 2nd sample proportion
#' @inheritParams two_sample_se
#' @export
two_prop_se <- function(phat1, n1, phat2, n2) {
  left <- (phat1 * (1 - phat1)) / n1
  right <- (phat2 * (1 - phat2)) / n2
  return(sqrt(left + right))
}

#' Two Proportions z-test
#'
#' @inheritParams two_prop_se
#' @inherit test_base
#' @seealso [stats::prop.test()]
#' @seealso \code{prop.test(table(data$X, data$Y))}
#' @export
#'
#' @examples
#' two_prop_z(27/75, 75, 130/556, 556, 1)
two_prop_z <- function(phat1, n1, phat2, n2, tail, conf = 0.95) {
  se <- two_prop_se(phat1, n1, phat2, n2)
  z <- ((phat1 - phat2) - 0) / se
  p <- z_test(z, tail)
  moe <- z_crit(conf) * se
  ci <- interval(abs(phat1 - phat2), moe)

  stats <- setNames(data.frame(se, z, p), c("se(ph1-ph2)", "z.stat", "p.value"))
  stats_print(
    method = "Two Proportion z-test", tail = tail,
    stats = stats, conf = conf, ci = ci, moe = moe
  )
}
