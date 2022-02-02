#' Standardize a normal distribution
#'
#' Transform a normal distribution to a standard normal distribution
#' @param x value
#' @param mu population mean
#' @param sigma population standard deviation
#'
#' @return standardized normal distribution
#' @export
stand_normal <- function(x, mu, sigma) {
  return((x - mu) / sigma)
}

#' Central Limit Theorem to Standardize a normal distribution
#'
#' If the population is not normally distributed, for sufficiently large n,
#' we use central limit theorem.
#' @param x the sample mean.
#' @param mu the population mean
#' @param sigma the population standard deviation
#' @param n the sample size
#'
#' @return standardized normal distribution
#' @export
central_limit <- function(x, mu, sigma, n) {
  return((x - mu) / (sigma / sqrt(n)))
}

#' Discrete Probability Distribution
#'
#' Calculates the Expected mean, variance and standard deviation for
#' a discrete probability distribution
#' @param x the outcome
#' @param prob the probability of the outcome
#'
#' @return the list of stats
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' discrete_dist(0:3, c(0.21, 0.45, 0.23, 0.11))
discrete_dist <- function(x, prob) {
  dist <- setNames(data.frame(x, prob), c("X", "P(X=x)"))
  print(dist, row.names = FALSE)

  ex <- sum(x * prob)
  varx <- sum(prob * ((x - ex)^2))
  sdx <- sqrt(varx)
  stats <- setNames(data.frame(ex, varx, sdx), c("E(X)", "Var(X)", "sd(X)"))
  stats_print(stats = stats, method = "Discrete Probability Distribution", dist = dist)
}

#' Binomial Distribution
#'
#' Calculates the Expected mean, variance and standard deviation for
#' a binomial distribution
#' @param n number of outcomes
#' @param prob the probability
#'
#' @return E(X), Var(X), sd(X)
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' binom_dist(5, 0.584)
binom_dist <- function(n, prob) {
  ex <- n * prob
  varx <- n * prob * (1 - prob)
  sdx <- sqrt(varx)
  dist <- sprintf("X ~ Binom(%g, %g)", n, prob)
  cat(dist, "\n")
  stats <- setNames(data.frame(ex, varx, sdx), c("E(X)", "Var(X)", "sd(X)"))
  stats_print(stats = stats, method = "Binomial Distribution (n, p)", dist = dist)
}

#' Sampling Distribution of the Sample Mean
#'
#' @param mu population mean
#' @param sigma population sd
#' @param n random sample size
#'
#' @return E(Xbar), Var(Xbar), sd(Xbar)
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' sampling_dist_mean(50, 8, 4)
sampling_dist_mean <- function(mu, sigma, n) {
  exb <- mu
  varxb <- (sigma^2) / n
  sdxb <- sigma / sqrt(n)
  dist <- sprintf("Xbar ~ Norm(%g, %g/sqrt(%g))", mu, sigma, n)
  cat(dist, "\n")
  stats <- setNames(data.frame(exb, varxb, sdxb), c("E(Xbar)", "Var(Xbar)", "sd(Xbar)"))
  stats_print(stats = stats, method = "Sampling Distribution of the Sample Mean)", dist = dist)
}

#' Sampling Distribution of the Sample Proportions
#'
#' @param p population proportion
#' @param n random sample size
#'
#' @return E(phat), Var(phat), sd(phat)
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' sampling_dist_prop(0.1, 10)
sampling_dist_prop <- function(p, n) {
  eph <- p
  varph <- (p * (1 - p)) / n
  sdph <- sqrt(varph)
  dist <- sprintf("Xbar ~ Norm(%g, %g)", p, sdph)
  cat(dist, "\n")
  stats <- setNames(data.frame(eph, varph, sdph), c("E(p.hat)", "Var(p.hat)", "sd(p.hat)"))
  stats_print(stats = stats, method = "Sampling Distribution of the Sample Proportions", dist = dist)
}
