test_that("Standard Normal Distribution", {
  delta_equal(stand_normal(145, 108, 15.87451), 2.330781)
})

test_that("Central Limit Theorm for Standardized Normal Form", {
  delta_equal(central_limit(60, 50, 8, 4), 2.5)
})

test_that("Discretre Probability Distribution", {
  x <- 0:3
  prob <- c(0.25, 0.20, 0.30, 0.25)
  capture_invisible(out <- discrete_dist(x, prob))
  expect_equal(out$dist[[1]], x)
  expect_equal(out$dist[[2]], prob)

  expect_equal(out$method, "Discrete Probability Distribution")
  delta_equal(out$`E(X)`, 1.55)
  delta_equal(out$`Var(X)`, 1.2475)
  delta_equal(out$`sd(X)`, sqrt(out$`Var(X)`))
})

test_that("Binomial Distribution", {
  n <- 5
  p <- 0.584
  capture_invisible(x <- binom_dist(n, p))
  expect_equal(x$dist, sprintf("X ~ Binom(%g, %g)", n, p))
  expect_equal(x$method, "Binomial Distribution (n, p)")
  delta_equal(x$`E(X)`, n * p)
  delta_equal(x$`Var(X)`, n * p * (1 - p))
  delta_equal(x$`sd(X)`, sqrt(n * p * (1 - p)))
})

test_that("Sampling Distribution of the Sample Mean", {
  capture_invisible(x <- sampling_dist_mean(50, 8, 4))
  expect_equal(x$dist, "Xbar ~ Norm(50, 8/sqrt(4))")
  expect_equal(x$method, "Sampling Distribution of the Sample Mean")
  expect_equal(x$`E(Xbar)`, 50)
  expect_equal(x$`Var(Xbar)`, 16)
  expect_equal(x$`sd(Xbar)`, 4)
})

test_that("Sampling Distribution of the Sample Proportions", {
  p <- 0.25
  n <- 25
  capture_invisible(x <- sampling_dist_prop(p, n))
  expect_equal(x$method, "Sampling Distribution of the Sample Proportions")
  expect_equal(x$`E(p.hat)`, p)
  expect_equal(x$`Var(p.hat)`, (p * (1 - p)) / n)
  expect_equal(x$`sd(p.hat)`, sqrt(x$`Var(p.hat)`))

  expect_equal(sampling_dist_prop_norm(0.34, p, n), (0.34 - p) / x$`sd(p.hat)`)
})
