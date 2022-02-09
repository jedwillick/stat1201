#' Simple Linear Regression t-stat
#'
#' @param b the sample estimate
#' @param se the standard error
#' @param B the population estimate
#'
#' @return the t-stat
#' @seealso [stats::lm()]
#' @seealso \code{lm(Y ~ X, data)}
#' @export
slr_t <- function(b, se, B = 0) {
  return((b - B) / se)
}

#' Simple Linear Regression Standard Error
#'
#' @param t the t-stat
#' @inheritParams slr_t
#'
#' @return the se
#' @export
slr_se <- function(b, t, B = 0) {
  return((b - B) / t)
}

#' Linear Regression Degrees of Freedom
#'
#' @param n the sample size
#' @param k the number of groups
#'
#' @return the df
#' @export
lr_df <- function(n, k) {
  return(n - k - 1)
}

#' Linear Regression Margin of Error
#'
#' @inherit moe_base
#' @export
lr_moe <- function(df, se, conf = 0.95) {
  return(t_crit(conf, df) * se)
}

#' Linear Regression Assumptions
#'
#' @inherit assumptions return description
#'
#' @seealso [stats::lm()]
#' @seealso \code{lm(Y ~ X, data)}
#' @seealso \code{lm(Y ~ X1 * X2, data) # Interaction}
#' @seealso \code{lm(Y ~ X1 + X2, data) # No Interaction}
#' @export
#'
#' @examples
#' lr_assumptions()
lr_assumptions <- function() {
  writeLines(c(
    "1. Linearity - Relationship between X and Y is linear.",
    "2. Errors are independent",
    "3. Normality of Residuals (errors)",
    "4. Equal variance of errors."
  ))
}

#' Adjusted R-Squared Value
#'
#' Calculates the adjusted R-Squared value for multiple Linear Regression
#' @param R2 the non-adjusted R-Squared
#' @param n the sample size
#' @param k the number of independent variables
#'
#' @return adjusted R-Squared
#' @export
#'
#' @examples
#' mlr_adjusted_R2(0.78, 20, 2)
mlr_adjusted_R2 <- function(R2, n, k) {
  return(1 - ((1 - R2) * ((n - 1) / (n - k - 1))))
}
