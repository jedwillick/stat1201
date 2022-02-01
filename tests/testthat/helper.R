#' expect_equal with default tolerance
#'
#' @inherit testthat::expect_equal
delta_equal <- function(..., tolerance = 1e-5) {
  expect_equal(..., tolerance = tolerance)
}

capture_invisible <- function(...) {
  invisible(capture.output(...))
}
