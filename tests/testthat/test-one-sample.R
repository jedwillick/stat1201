test_that("students se", {
  expect_equal(students_se(69.420, 21), 69.420 / sqrt(21))
})

test_that("students t lecture  example", {
  capture_invisible(x <- students_t(55, 60.8, 10.91, 20, 2))
  expect_equal(x$method, "2-Sided One Sample Student's t-test")
  expect_equal(x$tail, 2)
  expect_equal(x$df, 19)
  delta_equal(x$`se(x)`, 10.91 / sqrt(20))
  delta_equal(x$t.stat, 2.377488)
  delta_equal(x$p.value, 0.02808788)
  expect_equal(x$evidence, "moderate")
  moe <- t_crit(0.95, 19) * x$`se(x)`
  delta_equal(x$moe, moe)
  delta_equal(x$ci, interval(60.8, moe))
})
