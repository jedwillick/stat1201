test_that("Logisitic Regression Odds and Odds Ratio (OR)", {
  p1 <- 52 / 200
  p2 <- 18 / 200
  expect_equal(logistic_odds(p1), p1 / (1 - p1))
  expect_equal(logistic_odds(p2), p2 / (1 - p2))
  expect_equal(logistic_or(p1, p2), logistic_odds(p1) / logistic_odds(p2))
})

test_that("Logistic Regression Solve for estimated probability (phat)", {
  b0 <- -12.5974
  b1 <- 0.7630
  x <- 15
  delta_equal(logistic_phat(b0, b1, x), exp(b0 + (b1 * x)) / (1 + exp(b0 + (b1 * x))))
})

test_that("Logistic Regression to Compare Odds Between Groups", {
  b0 <- -2.3136
  b1 <- 1.2677
  delta_equal(logistic_groups(b0, b1), exp((b0 + b1) - b0))
})

test_that("Logistic Regression Margin of Error", {
  delta_equal(logistic_moe(0.2263), 0.2263 * z_crit(0.95))
})
