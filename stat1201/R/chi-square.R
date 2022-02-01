#' Generate data.frame
#'
#' Generates a data.frame from observed frequencies.
#' @param ... The observed frequencies (fo) in the form of \cr
#'            Col1 = c(Row1 = 52, Row2 = 61), Col2 = c(Row1 = 22, Row2 = 39)
#'
#' @return the new data.frame
#' @export
#'
#' @examples
#' generate_frame(Nicotine = c(No = 148, Yes = 52), Placebo = c(No = 182, Yes = 18))
#' generate_frame(Home = c("2018" = 391, "2020" = 454), Shared = c("2018" = 185, "2020" = 139))
generate_frame <- function(...) {
  fo <- data.frame(...)

  x <- data.frame(matrix(nrow = sum(fo), ncol = 2, dimnames = list(c(), c("Cols", "Rows"))))
  cols <- colnames(fo)
  rows <- rownames(fo)
  index <- 1
  for (i in seq_along(fo)) {
    for (j in seq_along(fo[[i]])) {
      for (k in index:(fo[[i]][j] + index - 1)) {
        x$Cols[k] <- cols[i]
        x$Rows[k] <- rows[j]
      }
      index <- index + fo[[i]][j]
    }
  }
  return(x)
}

#' Chi-Square Test for Independence
#' @param correct Boolean indicating whether to perform Yates' Continuity Correction
#' @inheritParams generate_frame
#'
#' @inherit stats::chisq.test return
#' @export
#' @importFrom stats chisq.test
#'
#' @examples
#' chisq_indep(Nicotine = c(No = 148, Yes = 52), Placebo = c(No = 182, Yes = 18))
#' chisq_indep(Home = c("2018" = 391, "2020" = 454), Shared = c("2018" = 185, "2020" = 139))
chisq_indep <- function(correct = FALSE, ...) {
  frame <- generate_frame(...)
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
