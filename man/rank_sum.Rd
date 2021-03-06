% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nonparametric.R
\name{rank_sum}
\alias{rank_sum}
\title{Rank-Sum z-test (Wilcoxon Rank-Sum test)}
\usage{
rank_sum(W, n1, n2, tail = 2)
}
\arguments{
\item{W}{sum of ranks for observations from the target sample.}

\item{n1}{the sample size whose ranks are summing}

\item{n2}{the sample size of the other group}

\item{tail}{specify either \code{1} or \code{2} tail}
}
\value{
the list of stats.
}
\description{
Calculates the statistics, prints in a nice format,
and returns a list for easy access of the stats.
}
\examples{
rank_sum(141.5, 10, 10, 1)
}
