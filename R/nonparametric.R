#' Signed-Rank Assumptions (Maybe Sum-Rank too?)
#'
#' @inherit assumptions return description
#' @export
rank_assumptions <- function() {
  writeLines(c(
    "Normal Approximation",
    "If the number of pairs is such that (n(n+1))/2 is large enough (> 20),
    a normal approximation can be used"
  ))
}

#' Signed-Rank Standard Deviation with Normal Approximation
#'
#' @param n the sample size
#'
#' @return standard deviation
#' @export
signed_rank_sd <- function(n) {
  return(sqrt((n * (n + 1) * ((2 * n) + 1)) / 24))
}

#' Signed-Rank Expected Mean with Normal Approximation
#'
#' @inheritParams signed_rank_sd
#'
#' @return expected mean
#' @export
signed_rank_expected <- function(n) {
  return((n * (n + 1)) / 4)
}

#' Signed-Rank test with Normal Approximation
#'
#' @param S sum of ranks corresponding to either positive or negative differences
#' @inheritParams signed_rank_sd
#' @inherit test_base
#' @export
#'
#' @examples
#' signed_rank(96, 15, 1)
signed_rank <- function(S, n, tail) {
  es <- signed_rank_expected(n)
  sds <- signed_rank_sd(n)
  z <- (S - es)/sds
  p <- z_test(z, tail)
  stats <- setNames(data.frame(S, es, sds, z, p), c("S", "E(S)", "sd(S)", "z.stat", "p.value"))
  stats_print(stats=stats, method="Signed Rank Test", tail=tail)
}

#' Rank-Sum Expected Mean With Normal Approximation
#'
#' @param n1 the sample size whose ranks are summing
#' @param n2 the sample size of the other group
#'
#' @return the expected mean
#' @export
rank_sum_expected <- function(n1, n2) {
  return((n1 * (n1+n2+1))/2)
}

#' Rank-Sum standard Deviation With Normal Approximation
#'
#' @inheritParams rank_sum_expected
#' @return the standard deviation
#' @export
rank_sum_sd <- function(n1, n2) {
  return(sqrt((n1 * n2 * (n1 + n2 + 1))/12))
}

#' Rank-Sum test (Wilcoxon Rank-Sum test)
#'
#' @param W sum of ranks for observations from the target sample.
#' @inheritParams rank_sum_expected
#' @inherit test_base
#' @export
#'
#' @examples
#' rank_sum(141.5, 10, 10, 1)
rank_sum <- function(W, n1, n2, tail) {
  ew <- rank_sum_expected(n1 ,n2)
  sdw <- rank_sum_sd(n1, n2)
  z <- (W - ew)/sdw
  p <- z_test(z, tail)
  stats <- setNames(data.frame(W, ew, sdw, z, p), c("W", "E(W)", "sd(W)", "z.stat", "p.value"))
  stats_print(stats=stats, method="Rank-Sum Test (Wilcoxon)", tail=tail)
}




