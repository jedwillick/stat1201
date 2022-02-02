test_that("Signed-Rank Expected", {
  delta_equal(signed_rank_expected(15), 60)
})

test_that("Signed-Rank SD", {
  delta_equal(signed_rank_sd(15), sqrt((15 * 16 * 31) / 24))
})

test_that("Signed-Rank z-test Lecture Example", {
  capture_invisible(x <- signed_rank(96, 15, 1))
  expect_equal(x$method, "1-Sided Signed Rank Test")
  expect_equal(x$tail, 1)
  expect_equal(x$S, 96)
  delta_equal(x$`E(S)`, signed_rank_expected(15))
  delta_equal(x$`sd(S)`, signed_rank_sd(15))
  delta_equal(x$z.stat, (96 - x$`E(S)`) / x$`sd(S)`)
  delta_equal(x$p.value, z_test(x$z.stat, 1))
  expect_equal(x$evidence, "moderate")
})

test_that("Rank-Sum Expected", {
  delta_equal(rank_sum_expected(10, 10), 105)
  delta_equal(rank_sum_expected(9, 14), 108)
})

test_that("Rank-Sum SD", {
  delta_equal(rank_sum_sd(10, 10), sqrt((10 * 10 * (10 + 10 + 1)) / 12))
  delta_equal(rank_sum_sd(9, 14), 15.87451)
})

test_that("Rank-Sum z-test Lecture Example", {
  capture_invisible(x <- rank_sum(141.5, 10, 10, 1))
  expect_equal(x$method, "1-Sided Rank-Sum Test (Wilcoxon)")
  expect_equal(x$tail, 1)
  expect_equal(x$W, 141.5)
  delta_equal(x$`E(W)`, 105)
  delta_equal(x$`sd(W)`, rank_sum_sd(10, 10))
  delta_equal(x$z.stat, (141.5 - x$`E(W)`) / x$`sd(W)`)
  delta_equal(x$p.value, z_test(x$z.stat, 1))
  expect_equal(x$evidence, "strong")
})

test_that("Rank-Sum z-test Past Exam Question", {
  capture_invisible(x <- rank_sum(145, 9, 14, 2))
  expect_equal(x$method, "2-Sided Rank-Sum Test (Wilcoxon)")
  expect_equal(x$tail, 2)
  expect_equal(x$W, 145)
  delta_equal(x$`E(W)`, 108)
  delta_equal(x$`sd(W)`, 15.87451)
  delta_equal(x$z.stat, 2.330781)
  delta_equal(x$p.value, 0.01976491)
  expect_equal(x$evidence, "moderate")
})
