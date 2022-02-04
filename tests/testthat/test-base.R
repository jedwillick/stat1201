test_that("Mode", {
  expect_equal(mode_stats(c(80, 70, 66, 50, 66, 74, 78, 58)), 66)
})

test_that("Population SD", {
  x <- c(10, 12, 13, 14, 15, 14, 15)
  mu <- mean(x)
  N <- length(x)
  expect_equal(population_sd(x, mu, N), sqrt(sum((x - mu)^2) / N))
})

test_that("Sample SD", {
  x <- c(10, 12, 13, 14, 15, 14, 15)
  expect_equal(sample_sd(x), sd(x))
})

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
  delta_equal(interval(45, z_crit(0.95) * 8 / sqrt(16)), c(41.08007, 48.91993))
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

test_that("Generate Frame Lecture Example", {
  frame <- generate_frame(Nicotine = c(No = 148, Yes = 52), Placebo = c(No = 182, Yes = 18))
  expect_equal(nrow(frame), 400)
  expect_equal(levels(frame$Cols), c("Nicotine", "Placebo"))
  expect_equal(levels(frame$Rows), c("No", "Yes"))

  tab <- addmargins(table(frame$Rows, frame$Cols))
  expect_equal(tab[1], 148)
  expect_equal(tab[2], 52)
  expect_equal(tab[3], 200)

  expect_equal(tab[4], 182)
  expect_equal(tab[5], 18)
  expect_equal(tab[6], 200)

  expect_equal(tab[7], 330)
  expect_equal(tab[8], 70)
  expect_equal(tab[9], 400)
})

test_that("Generate Frame Tut Example", {
  frame <- generate_frame(Home = c("2018" = 391, "2020" = 454), Shared = c("2018" = 185, "2020" = 139))
  expect_equal(nrow(frame), 1169)
  expect_equal(levels(frame$Cols), c("Home", "Shared"))
  expect_equal(levels(frame$Rows), c("2018", "2020"))

  tab <- addmargins(table(frame$Rows, frame$Cols))
  expect_equal(tab[1], 391)
  expect_equal(tab[2], 454)
  expect_equal(tab[3], 845)

  expect_equal(tab[4], 185)
  expect_equal(tab[5], 139)
  expect_equal(tab[6], 324)

  expect_equal(tab[7], 576)
  expect_equal(tab[8], 593)
  expect_equal(tab[9], 1169)
})
