test_that("SLR t-stat from estimate and se", {
  delta_equal(slr_t(1.2774, 0.3388), 3.770366)
})

test_that("SLR se from estimate and t-stat", {
  delta_equal(slr_se(63.5456, 5.003), 63.5456/5.003)
})

test_that("Linear regression degrees of freedom", {
  expect_equal(lr_df(20, 1), 18)
  expect_equal(lr_df(25, 1), 23)
  expect_equal(lr_df(100, 2), 97)
})
