test_that("SLR t-stat from estimate and se", {
  delta_equal(slr_t(1.2774, 0.3388), 3.770366)
})

test_that("SLR se from estimate and t-stat", {
  delta_equal(slr_se(63.5456, 5.003), 63.5456 / 5.003)
})

test_that("Linear regression degrees of freedom", {
  expect_equal(lr_df(20, 1), 18)
  expect_equal(lr_df(25, 1), 23)
  expect_equal(lr_df(100, 2), 97)
})

test_that("Linear Regression Margin of Error", {
  delta_equal(lr_moe(97, 14.52, conf = 0.9), t_crit(0.9, 97) * 14.52)
})

test_that("Adjusted R-Squared", {
  delta_equal(mlr_adjusted_R2(0.78, 20, 2), 1 - ((1 - 0.78) * ((20 - 1) / (20 - 2 - 1))))
})
