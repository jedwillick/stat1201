#' Proportion Margin of Error
#'
#' @inherit moe_base
#' @export
prop_moe <- function(se, conf = 0.95) {
  return(z_crit(conf) * se)
}


#' One Sample Proportion Standard Error
#'
#' @param phat Sample proportion
#' @param n Sample size
#'
#' @return Standard Error
#' @export
#'
#' @examples
#' one_prop_se(10 / 15, 15)
one_prop_se <- function(phat, n) {
  return(sqrt((phat * (1 - phat)) / n))
}


#' One Sample Proportion z-test
#'
#' @inheritParams one_prop_se
#' @param p population proportion
#' @inherit test_base
#' @export
one_prop_z <- function(phat, n, p = 0, tail = 2, conf = 0.95) {
  se <- one_prop_se(phat, n)
  z <- (phat - p) / se
  p <- z_test(z, tail)
  moe <- z_crit(conf) * se
  ci <- interval(phat, moe)

  stats <- setNames(data.frame(se, z, p), c("se(p)", "z.stat", "p.value"))
  stats_print(
    method = "One Sample Proportion z-test", tail = tail,
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
#' two_prop_z(27 / 75, 75, 130 / 556, 556, 1)
two_prop_z <- function(phat1, n1, phat2, n2, tail = 2, conf = 0.95) {
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
