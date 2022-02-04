test_that("Proportion MOE", {
  expect_equal(prop_moe(3.4562, conf = 0.8), z_crit(0.8) * 3.4562)
  expect_equal(prop_moe(3.4562, conf = 0.9), z_crit(0.9) * 3.4562)
})

test_that("One Proportion z-test", {
  n <- 15
  phat <- 10 / n
  p <- 0.5

  expect_equal(one_prop_se(phat, n), sqrt((phat * (1 - phat)) / n))

  capture_invisible(x <- one_prop_z(phat, n, p = p, tail = 1, conf = 0.9))
  expect_equal(x$method, "1-Sided One Sample Proportion z-test")

  delta_equal(x$`se(p)`, one_prop_se(phat, n))
  delta_equal(x$z.stat, (phat - p) / x$`se(p)`)
  delta_equal(x$p.value, z_test(x$z.stat, 1))
  expect_equal(x$evidence, "weak")
  expect_equal(x$tail, 1)
  expect_equal(x$conf, 0.9)
  delta_equal(x$moe, z_crit(0.9) * x$`se(p)`)
  expect_equal(x$ci, interval(phat, x$moe))
})

test_that("Two proportion SE", {
  delta_equal(two_prop_se(0.32, 100, 0.18, 100), 0.06043178)
})

test_that("Two Proportions z-test Lecture example", {
  capture_invisible(x <- two_prop_z(0.32, 100, 0.18, 100, 1))
  expect_equal(x$method, "1-Sided Two Proportion z-test")
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
