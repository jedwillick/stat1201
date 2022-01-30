delta <- 1e-5

test_that("not in", {
  expect_true(0 %ni% c(1, 2))
  expect_false(2 %ni% c(1, 2))
})


test_that("p value evidence", {
  expect_equal(p_evidence(0.1), "no")
  expect_equal(p_evidence(0.05), "weak")
  expect_equal(p_evidence(0.01), "moderate")
  expect_equal(p_evidence(0.009), "strong")
})

test_that("Z critical", {
  expect_equal(z_crit(0.95), qnorm(0.975))
  expect_equal(z_crit(0.9), qnorm(0.95))
})

test_that("t critical", {
  expect_equal(t_crit(0.95, 10), qt(0.975, 10))
  expect_equal(t_crit(0.9, 2), qt(0.95, 2))
})

test_that("Confidence Interval", {
  expect_equal(ci_interval(10.12, 2.4), c(10.12 - 2.4, 10.12 + 2.4))
  expect_equal(ci_interval(45, z_crit(0.95) * 8 / sqrt(16)), c(41.08007, 48.91993), tolerance = delta)
})

test_that("outliers", {
  expect_output(outliers(6.45, 10.23), "Observation < 0.78")
  expect_output(outliers(6.45, 10.23), "Observation > 15.9")
  expect_equal(outliers(6.45, 10.23, obs = 0.5), "Outlier to the left: 0.5 < 0.78")
  expect_equal(outliers(6.45, 10.23, obs = 16), "Outlier to the right: 16 > 15.9")
  expect_equal(outliers(6.45, 10.23, obs = 12), "12 is not an outlier!")
})

test_that("t-test", {
  expect_error(t_test(1.232, 4, 0))
  expect_error(t_test(1.232, 4, 3))
  expect_equal(t_test(2.324, 4, 2), 2 * (1 - pt(2.324, 4)))
  expect_equal(t_test(2.324, 4, 1), 1 - pt(2.324, 4))
  expect_equal(t_test(-3.42, 4, 2), 2 * pt(-3.42, 4))
  expect_equal(t_test(-3.42, 4, 1), pt(-3.42, 4))
})

test_that("z-test", {
  expect_error(z_test(1.232, 0))
  expect_error(z_test(1.232, 3))
  expect_equal(z_test(2.324, 2), 2 * (1 - pnorm(2.324)))
  expect_equal(z_test(2.324, 1), 1 - pnorm(2.324))
  expect_equal(z_test(-3.42, 2), 2 * pnorm(-3.42))
  expect_equal(z_test(-3.42, 1), pnorm(-3.42))
})

test_that("students se", {
  expect_equal(students_se(69.420, 21), 69.420 / sqrt(21))
})

test_that("students t lecture 5 example", {
  invisible(capture.output(out <- students_t(60.8, 55, 10.91, 20, 2)))
  expect_equal(out[[1]], "2-Sided One Sample Student's t-test")

  test <- out[[2]]
  expect_equal(test$df, 19)
  expect_equal(test$`se(x)`, 10.91 / sqrt(20), tolerance = delta)
  expect_equal(test$t.stat, 2.377488, tolerance = delta)
  expect_equal(test$p.value, 0.02808788, tolerance = delta)
  expect_equal(test$evidence, "moderate")
  ci = ci_interval(60.8, t_crit(0.95, 19) * test$`se(x)`)
  expect_equal(out[[3]], sprintf("95%% CI: (%g, %g)", ci[1], ci[2]))
})

test_that("two sample se", {
  expect_equal(two_sample_se(6.12, 12, 7.39, 11), sqrt(((6.12 ^ 2) / 12) + ((7.39 ^ 2) / 11)))
})

test_that("two sample t-test (SD's not equal) lecture 7 example", {
  invisible(capture.output(out <- two_sample_t(50.19, 17.91, 10, 26.18, 7.29, 10, 2)))
  expect_equal(out[[1]], "2-Sided Two Sample t-test SD's not equal")

  test <- out[[2]]
  expect_equal(test$df, 9)
  expect_equal(test$`se(x1-x2)`, 6.114836, tolerance = delta)
  expect_equal(test$t.stat, 3.926516, tolerance = delta)
  expect_equal(test$p.value, 0.0034764, tolerance = delta)
  expect_equal(test$evidence, "strong")

  expect_equal(out[[3]], "95% CI: (10.1773, 37.8427)")
})

test_that("Pooled variance", {
  expect_equal(pooled_S2p(6.3, 25, 6.1, 20), 38.59419, tolerance = delta)
})

test_that("Pooled se", {
  expect_equal(pooled_se(6.3, 25, 6.1, 20), 1.863727, tolerance = delta)
})


test_that("Pooled t-test", {
  invisible(capture.output(out <- pooled_t(172.26, 6.3, 25, 167.32, 6.1, 20, 1, 0.9)))
  expect_equal(out[[1]], "1-Sided Pooled t-test SD's equal")

  test <- out[[2]]
  expect_equal(test$df, 43)
  expect_equal(test$S2p, 38.59419, tolerance = delta)
  expect_equal(test$`se(x1-x2)`, 1.863727, tolerance = delta)
  expect_equal(test$t.stat, 2.650603, tolerance = delta)
  expect_equal(test$p.value, 0.0056003, tolerance = delta)
  expect_equal(test$evidence, "strong")

  expect_equal(out[[3]], sprintf("90%% CI: (%g, %g)", 4.94 - 3.133057, 4.94 + 3.133057))
})


