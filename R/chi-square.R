#' Chi-Square Test for Independence
#' @inheritParams generate_frame
#' @param correct Boolean indicating whether to perform Yates' Continuity Correction
#'
#' @inherit stats::chisq.test return
#' @export
#' @importFrom stats chisq.test
#'
#' @examples
#' chisq_indep(Nicotine = c(No = 148, Yes = 52), Placebo = c(No = 182, Yes = 18))
#' chisq_indep(Home = c("2018" = 391, "2020" = 454), Shared = c("2018" = 185, "2020" = 139))
chisq_indep <- function(..., as_factors = TRUE, correct = FALSE) {
  frame <- generate_frame(..., as_factors = as_factors)
  chisq.test(table(frame$Rows, frame$Cols), correct = correct)
}


#' Chi-Square Independence Degrees of Freedom
#'
#' @param num.rows Number of rows
#' @param num.cols Number of columns
#'
#' @return degrees of freedom
#' @export
chisq_indep_df <- function(num.rows, num.cols) {
  return((num.rows - 1) * (num.cols - 1))
}


#' Chi-Square Independence Expected Frequency
#'
#' @param RowTotal the row total
#' @param ColTotal the column total
#' @param GrandTotal the grand total
#'
#' @return the expected frequency for the specified cell
#' @export
chisq_indep_expected <- function(RowTotal, ColTotal, GrandTotal) {
  return((RowTotal * ColTotal) / GrandTotal)
}

#' Chi-Square Goodness of Fit Test
#'
#' Calculates the frequency table, and the statistics, prints in a nice format,
#' and returns a list for easy access of the stats.
#' @param fo the observed frequencies.
#' @param p the probability of each outcome.
#' @param k the number of parameters estimated using the data.
#'
#' @inherit stats_print return
#' @export
#'
#' @examples
#' chisq_gof(c(185, 190, 210, 205, 195, 215), 1 / 6)
#' chisq_gof(c(Low = 100, Med = 255, High = 45), c(0.2, 0.7, 0.1))
chisq_gof <- function(fo, p, k = 0) {
  len <- length(fo)

  x <- data.frame(fo, fe = sum(fo) * p)
  x$chi <- ((x$fo - x$fe)^2) / x$fe

  df <- len - k - 1
  xsq.stat <- sum(x$chi)
  p.value <- xsq_test(xsq.stat, df)
  stats <- data.frame(df, xsq.stat, p.value)

  print(x)
  stats_print(method = "Chi-Square Goodness of Fit Test", stats = stats, freq = x)
}


#' Chi-Square X^2-test
#'
#' @param xsq the x^2 stat
#' @param df the degrees of freedom
#'
#' @return the p value
#' @export
#' @importFrom stats pchisq
xsq_test <- function(xsq, df) {
  return(1 - pchisq(abs(xsq), df))
}
