test_that("ANOVA Mean Square Group", {
  expect_equal(anova_msg(117.1, 4), 117.1 / 3)
})

test_that("ANOVA Mean Square Residuals", {
  expect_equal(anova_msr(312.5, 27, 4), 312.5 / 23)
})

test_that("ANOVA Mean Square Total", {
  expect_equal(anova_mst(429.6, 27), 429.6 / 26)
})

test_that("ANOVA f-stat", {
  delta_equal(anova_f(anova_msg(117.1, 4), anova_msr(312.5, 27, 4)), 2.8728533)
})

test_that("ANOVA f-test", {
  f <- anova_f(anova_msg(117.1, 4), anova_msr(312.5, 27, 4))
  evid <- capture_invisible(x <- f_test(f, 3, 23))
  delta_equal(x, 0.058286)
  expect_equal(evid, "weak evidence")
})
