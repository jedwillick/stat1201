test_that("Assumptions", {
  expect_output(two_sample_assumptions())
  expect_output(two_prop_assumptions())
  expect_output(lr_assumptions())
  expect_output(anova_assumptions())
  expect_output(rank_assumptions())
})
