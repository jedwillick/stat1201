#' ANOVA Mean Squares Group
#'
#' @param ssg The Sum of Squares Group
#' @param k The number of groups
#' @return the mean squares for the group
#' @export
anova_msg <- function(ssg, k) {
  return(ssg / (k - 1))
}

#' ANOVA Mean Squares Residuals
#'
#' @param ssr the Sum of Squares Residuals
#' @param n the sample size
#' @inherit anova_msg
#'
#' @return the mean squares for the residuals
#' @export
anova_msr <- function(ssr, n, k) {
  return(ssr / (n - k))
}

#' ANOVA Mean Squares Total
#'
#' @param sst the Sum of Squares Total
#' @inherit anova_msr
#'
#' @return the mean squares Total
#' @export
anova_mst <- function(sst, n) {
  return(sst / (n - 1))
}

#' ANOVA f-stat from msg and msr
#'
#' @param msg Mean Squares Group
#' @param msr Mean Squares Residual
#'
#' @return f-stat
#' @export
anova_f <- function(msg, msr) {
  return(msg / msr)
}

#' ANOVA f-test
#'
#' @param f the f-stat
#' @param df1 group degrees of freedom (k - 1)
#' @param df2 residuals degrees of freedom (n - k)
#'
#' @return p-value
#' @export
#' @importFrom stats pf
#' @seealso \code{stats::aov(Y ~ X, data) # One-Way}
#' @seealso \code{stats::aov(Y ~ X1 * X2, data) # Two-Way}
#' @seealso \code{stats::pairwise.t.test(x, g, p.adjust.method = "none")}
#' @seealso \code{stats::pairwise.t.test(x, g, p.adjust.method = "bonferroni") # significance = 0.05/numPairs}
#' @seealso \code{stats::TukeyHSD(fit)}
f_test <- function(f, df1, df2) {
  p <- 1 - pf(abs(f), df1, df2)
  cat(p_evidence(p), "evidence\n")
  return(1 - pf(abs(f), df1, df2))
}

#' ANOVA Assumptions
#'
#' @inherit assumptions return description
#' @export
anova_assumptions <- function() {
  writeLines(c(
    "1. Observations are random and independent.",
    "2. Observations in each sample group are drawn from approximately normally distributed populations. Is robust to violations.",
    "3. Population variances of the groups are equal (Homogeneity of Variances)."
  ))
}
