#' Odds
#'
#' @param P_event Probability of event
#'
#' @return Odds of event
#' @export
logistic_odds <- function(P_event) {
  return(P_event(1 - P_event))
}

#' Odds Ratio (OR)
#'
#' @param P1 Probability of event 1
#' @param P2 Probability of event 2
#'
#' @return Odds Ratio
#' @export
logistic_or <- function(P1, P2) {
  return(P1 / P2)
}

#' Logistic Regression phat
#'
#' @param b0 the estimate intercept
#' @param b1 the estimate of the variable
#' @param x the value being predicted
#'
#' @return estimated probability.
#' @export
#'
#' @examples
#' logistic_phat(-12.5974, 0.7630, 15)
logistic_phat <- function(b0, b1, x) {
  numerator <- exp(b0 + (b1 * x))
  denominator <- 1 + exp(b0 + (b1 * x))
  return(numerator / denominator)
}


#' Logistic Regression Margin of Error
#'
#' Logistic Regression is a z-distribution.
#' @param se the standard error
#' @param conf the confidence
#'
#' @return the margin of error
#' @export
logistic_moe <- function(se, conf = 0.95) {
  return(z_crit(conf) * se)
}

#' Logistic Regression to Compare Odds Between Groups
#'
#' @inheritParams logistic_phat
#'
#' @return Odds Ratio between groups
#' @export
#'
#' @examples
#' logistic_groups(-2.3136, 1.2677)
logistic_groups <- function(b0, b1) {
  first <- b0 + (b1 * 1)
  second <- b0 + (b1 * 0)
  return(exp(first - second))
}
