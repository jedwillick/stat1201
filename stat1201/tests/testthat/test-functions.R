deltaEqual <- function(...) {
  expect_equal(..., tolerance = 1e-5)
}

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
  expect_equal(interval(10.12, 2.4), c(10.12 - 2.4, 10.12 + 2.4))
  deltaEqual(interval(45, z_crit(0.95) * 8 / sqrt(16)), c(41.08007, 48.91993))
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
  invisible(capture.output(x <- students_t(60.8, 55, 10.91, 20, 2)))
  expect_equal(x$title, "2-Sided One Sample Student's t-test")
  expect_equal(x$tail, 2)
  expect_equal(x$df, 19)
  deltaEqual(x$`se(x)`, 10.91 / sqrt(20))
  deltaEqual(x$t.stat, 2.377488)
  deltaEqual(x$p.value, 0.02808788)
  expect_equal(x$evidence, "moderate")
  moe = t_crit(0.95, 19) * x$`se(x)`
  deltaEqual(x$moe, moe)
  deltaEqual(x$ci, interval(60.8, moe))
})

test_that("two sample se", {
  expect_equal(two_sample_se(6.12, 12, 7.39, 11), sqrt(((6.12 ^ 2) / 12) + ((7.39 ^ 2) / 11)))
})

test_that("two sample t-test (SD's not equal) lecture 7 example", {
  invisible(capture.output(x <- two_sample_t(50.19, 17.91, 10, 26.18, 7.29, 10, 2)))
  expect_equal(x$title, "2-Sided Two Sample t-test SD's Not Equal")
  expect_equal(x$tail, 2)
  expect_equal(x$df, 9)
  deltaEqual(x$`se(x1-x2)`, 6.114836)
  deltaEqual(x$t.stat, 3.926516)
  deltaEqual(x$p.value, 0.0034764)
  expect_equal(x$evidence, "strong")
  moe = t_crit(0.95, 9) * x$`se(x1-x2)`
  deltaEqual(x$moe, moe)
  deltaEqual(x$ci, interval(50.19 - 26.18, moe))
})

test_that("Pooled variance", {
  deltaEqual(pooled_S2p(6.3, 25, 6.1, 20), 38.59419)
})

test_that("Pooled se", {
  deltaEqual(pooled_se(6.3, 25, 6.1, 20), 1.863727)
})


test_that("Pooled t-test Lec 7 example", {
  invisible(capture.output(x <- pooled_t(172.26, 6.3, 25, 167.32, 6.1, 20, 1, 0.9)))
  expect_equal(x$title, "1-Sided Pooled t-test SD's Equal")
  expect_equal(x$tail, 1)
  expect_equal(x$df, 43)
  deltaEqual(x$S2p, 38.59419)
  deltaEqual(x$`se(x1-x2)`, 1.863727)
  deltaEqual(x$t.stat, 2.650603)
  deltaEqual(x$p.value, 0.0056003)
  expect_equal(x$evidence, "strong")
  expect_equal(x$conf, 0.9)
  moe = t_crit(0.9, 43) * x$`se(x1-x2)`
  deltaEqual(x$moe, moe)
  deltaEqual(x$ci, interval(4.94, moe))
})

test_that("Two proportion SE", {
  deltaEqual(two_proportions_se(0.32, 100, 0.18, 100), 0.06043178)
})

test_that("Two Proportions z-test Lec 7 example", {
  invisible(capture.output(x <- two_proportions_z(0.32, 100, 0.18, 100, 1)))
  expect_equal(x$title, "1-Sided Two Proportion z-test")
  expect_equal(x$tail, 1)
  deltaEqual(x$`se(ph1-ph2)`, 0.06043178)
  deltaEqual(x$z.stat, 2.316662)
  deltaEqual(x$p.value, 0.010261)
  expect_equal(x$evidence, "moderate")
  expect_equal(x$conf, 0.95)
  moe = z_crit(0.95) * x$`se(ph1-ph2)`
  deltaEqual(x$moe, moe)
  deltaEqual(x$ci, interval(0.32-0.18, moe))
})

test_that("Correlation SE", {
  deltaEqual(correlation_se(0.6642512, 20), 0.1761897)
})

test_that("Correlation t-test Lec8 example", {
  invisible(capture.output(x <- correlation_t(0.6642512, 20, 2)))
    expect_equal(x$title, "2-Sided Correlation t-test")
  expect_equal(x$tail, 2)
  expect_equal(x$df, 18)
  deltaEqual(x$`se(r)`, 0.1761897)
  deltaEqual(x$t.stat, 3.770091)
  deltaEqual(x$p.value, 0.00140178)
  expect_equal(x$evidence, "strong")
  expect_equal(x$conf, 0.95)
  moe = t_crit(0.95, 18) * x$`se(r)`
  deltaEqual(x$moe, moe)
  deltaEqual(x$ci, interval(0.6642512, moe))
})