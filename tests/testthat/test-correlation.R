test_that("Correlation sd & df", {
  delta_equal(correlation_se(0.6642512, 20), 0.1761897)
  expect_equal(correlation_df(20), 18)
})

test_that("Correlation t-test Lecture example", {
  capture_invisible(x <- correlation_t(0.6642512, 20, 2))
  expect_equal(x$method, "2-Sided Correlation t-test")
  expect_equal(x$tail, 2)
  expect_equal(x$df, 18)
  delta_equal(x$`se(r)`, 0.1761897)
  delta_equal(x$t.stat, 3.770091)
  delta_equal(x$p.value, 0.00140178)
  expect_equal(x$evidence, "strong")
  expect_equal(x$conf, 0.95)
  moe <- t_crit(0.95, 18) * x$`se(r)`
  delta_equal(x$moe, moe)
  delta_equal(x$ci, interval(0.6642512, moe))
})
