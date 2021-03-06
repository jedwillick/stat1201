% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi-square.R
\name{chisq_indep}
\alias{chisq_indep}
\title{Chi-Square Test for Independence}
\usage{
chisq_indep(..., as_factors = TRUE, correct = FALSE)
}
\arguments{
\item{...}{The observed frequencies (fo) in the form of \cr
Col1 = c(Row1 = 52, Row2 = 61), Col2 = c(Row1 = 22, Row2 = 39)}

\item{as_factors}{Boolean specifying whether to treat the Cols and Rows as factors}

\item{correct}{Boolean indicating whether to perform Yates' Continuity Correction}
}
\value{
A list with class \code{"htest"} containing the following
  components:
  \item{statistic}{the value the chi-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic, \code{NA} if the
    p-value is computed by Monte Carlo simulation.}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test
    performed, and whether Monte Carlo simulation or continuity
    correction was used.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
  \item{residuals}{the Pearson residuals,
    \code{(observed - expected) / sqrt(expected)}.}
  \item{stdres}{standardized residuals,
    \code{(observed - expected) / sqrt(V)}, where \code{V} is the residual cell variance (Agresti, 2007,
    section 2.4.5 for the case where \code{x} is a matrix, \code{n * p * (1 - p)} otherwise).}
}
\description{
Chi-Square Test for Independence
}
\examples{
chisq_indep(Nicotine = c(No = 148, Yes = 52), Placebo = c(No = 182, Yes = 18))
chisq_indep(Home = c("2018" = 391, "2020" = 454), Shared = c("2018" = 185, "2020" = 139))
}
\seealso{
\code{\link[stats:chisq.test]{stats::chisq.test()}}
}
