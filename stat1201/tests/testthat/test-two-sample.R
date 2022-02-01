test_that("two sample se", {
  expect_equal(two_sample_se(6.12, 12, 7.39, 11), sqrt(((6.12^2) / 12) + ((7.39^2) / 11)))
})

test_that("two sample t-test (SD's not equal) lecture 7 example", {
  invisible(capture.output(x <- two_sample_t(50.19, 17.91, 10, 26.18, 7.29, 10, 2)))
  expect_equal(x$title, "2-Sided Two Sample t-test SD's Not Equal")
  expect_equal(x$tail, 2)
  expect_equal(x$df, 9)
  delta_equal(x$`se(x1-x2)`, 6.114836)
  delta_equal(x$t.stat, 3.926516)
  delta_equal(x$p.value, 0.0034764)
  expect_equal(x$evidence, "strong")
  moe <- t_crit(0.95, 9) * x$`se(x1-x2)`
  delta_equal(x$moe, moe)
  delta_equal(x$ci, interval(50.19 - 26.18, moe))
})

test_that("Pooled variance", {
  delta_equal(pooled_S2p(6.3, 25, 6.1, 20), 38.59419)
})

test_that("Pooled se", {
  delta_equal(pooled_se(6.3, 25, 6.1, 20), 1.863727)
})


test_that("Pooled t-test Lec 7 example", {
  invisible(capture.output(x <- pooled_t(172.26, 6.3, 25, 167.32, 6.1, 20, 1, 0.9)))
  expect_equal(x$title, "1-Sided Pooled t-test SD's Equal")
  expect_equal(x$tail, 1)
  expect_equal(x$df, 43)
  delta_equal(x$S2p, 38.59419)
  delta_equal(x$`se(x1-x2)`, 1.863727)
  delta_equal(x$t.stat, 2.650603)
  delta_equal(x$p.value, 0.0056003)
  expect_equal(x$evidence, "strong")
  expect_equal(x$conf, 0.9)
  moe <- t_crit(0.9, 43) * x$`se(x1-x2)`
  delta_equal(x$moe, moe)
  delta_equal(x$ci, interval(4.94, moe))
})

test_that("Two proportion SE", {
  delta_equal(two_proportions_se(0.32, 100, 0.18, 100), 0.06043178)
})

test_that("Two Proportions z-test Lec 7 example", {
  invisible(capture.output(x <- two_proportions_z(0.32, 100, 0.18, 100, 1)))
  expect_equal(x$title, "1-Sided Two Proportion z-test")
  expect_equal(x$tail, 1)
  delta_equal(x$`se(ph1-ph2)`, 0.06043178)
  delta_equal(x$z.stat, 2.316662)
  delta_equal(x$p.value, 0.010261)
  expect_equal(x$evidence, "moderate")
  expect_equal(x$conf, 0.95)
  moe <- z_crit(0.95) * x$`se(ph1-ph2)`
  delta_equal(x$moe, moe)
  delta_equal(x$ci, interval(0.32 - 0.18, moe))
})
