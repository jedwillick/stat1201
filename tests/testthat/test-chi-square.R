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

test_that("Chi-Square Test For Independence Lecture Example", {
  x <- chisq_indep(Nicotine = c(No = 148, Yes = 52), Placebo = c(No = 182, Yes = 18))

  fe <- x$expected
  expect_equal(fe[1], 165)
  expect_equal(fe[2], 35)
  expect_equal(fe[3], 165)
  expect_equal(fe[4], 35)

  expect_equal(x$parameter[[1]], 1)
  delta_equal(x$statistic[[1]], 20.01732)
  delta_equal(x$p.value[[1]], xsq_test(20.01732, 1))


})

test_that("Chi-Square Test For Independence Tut Example", {
  x <- chisq_indep(Home = c("2018" = 391, "2020" = 454), Shared = c("2018" = 185, "2020" = 139))

  fe <- x$expected
  expect_equal(fe[1], chisq_indep_expected(576, 845, 1169))
  expect_equal(fe[2], chisq_indep_expected(593, 845, 1169))
  expect_equal(fe[3], chisq_indep_expected(576, 324, 1169))
  expect_equal(fe[4], chisq_indep_expected(593, 324, 1169))

  expect_equal(x$parameter[[1]], 1)
  delta_equal(x$statistic[[1]], 10.983)
  delta_equal(x$p.value[[1]], xsq_test(10.983, 1))
})


test_that("Chi-Square Independence Degrees of Freedom", {
  expect_equal(chisq_indep_df(2, 2), 1)
  expect_equal(chisq_indep_df(4, 2), 3)
})

test_that("Chi-Square Independence Expected Freq", {
  expect_equal(chisq_indep_expected(330, 200, 400), 165)
  expect_equal(chisq_indep_expected(70, 200, 400), 35)
})

test_that("Chi-Square Goodness of Fit Test Lecture Example", {
  capture_invisible(x <- chisq_gof(c(185, 190, 210, 205, 195, 215), 1 / 6))

  freq = x$freq
  expect_equal(rownames(freq), as.character(1:6))
  expect_equal(freq$fo, c(185, 190, 210, 205, 195, 215))
  expect_equal(freq$fe, replicate(6, 200))
  expect_equal(freq$chi, c(1.125, 0.5, 0.5, 0.125, 0.125, 1.125))

  expect_equal(x$method, "Chi-Square Goodness of Fit Test")
  expect_equal(x$df, 6 - 0 - 1)
  expect_equal(x$xsq.stat, 3.5)
  delta_equal(x$p.value, xsq_test(3.5, 5))
  expect_equal(x$evidence, "no")
})

test_that("Chi-Square Goodness of Fit Test Lecture Example", {
  capture_invisible(x <- chisq_gof(c(Low = 100, Med = 255, High = 45), c(0.2, 0.7, 0.1)))

  freq = x$freq
  expect_equal(rownames(freq), c("Low", "Med", "High"))
  expect_equal(freq$fo, c(100, 255, 45))
  expect_equal(freq$fe, c(400 * 0.2, 400 * 0.7, 400 * 0.1))
  delta_equal(freq$chi, c(5, 2.232143, 0.625))

  expect_equal(x$method, "Chi-Square Goodness of Fit Test")
  expect_equal(x$df, 3 - 0 - 1)
  delta_equal(x$xsq.stat, 7.857143)
  delta_equal(x$p.value, xsq_test(7.857143, 2))
  expect_equal(x$evidence, "moderate")
})

test_that("X^2-test", {
  delta_equal(xsq_test(6.121, 4), 0.1902923)
  delta_equal(xsq_test(-1.2324, 6), 0.9752541)
})
