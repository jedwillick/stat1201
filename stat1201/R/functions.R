`%ni%` <- Negate(`%in%`)

p_evidence <- function(p) {
  if (p < 0.01) {
    return("Strong")
  } else if (p < 0.05) {
    return("Moderate")
  } else if (p < 0.1) {
    return("Weak")
  } else {
    return("No")
  }
}

z_crit <- function(conf) {
  return(qnorm(1 - ((1 - conf) / 2)))
}

t_crit <- function(conf, df) {
  return(qt(1 - ((1 - conf) / 2), df))
}

ci_interval <- function(x, moe) {
  return(c(x - moe, x + moe))
}

outliers <- function(Q1, Q3) {
  IQR = Q3 - Q1
  cat("Observation <", Q1 - (1.5 * IQR), "\n")
  cat("Observation >", Q3 + (1.5 * IQR), "\n")
}

t_test <- function(t, df, tail) {
  if (tail %ni% c(1, 2)) stop("Tail must be 1 or 2.")
  return(tail * (1 - pt(abs(t), df)))
}

z_test <- function(z, tail) {
  if (tail %ni% c(1, 2)) stop("Tail must be 1 or 2.")
  return(tail * (1 - pnorm(abs(z))))
}

students_se <- function(s, n) {
  return(s / sqrt(n))
}

students_t <- function(x, mu, s, n, tail, conf = 0.95) {
  df = n - 1
  se = students_se(s, n)
  t = (x - mu) / se
  p = t_test(t, df, tail)
  ci = ci_interval(x, t_crit(conf, df) * se)
  tab = as.data.frame(matrix(c(df, se, t, p, p_evidence(p)), ncol = 5, dimnames = list(c(sprintf("%d-side", tail)), c('df', 'se(x)', 't_stat', 'p_value', 'evidence'))))

  print(list(sprintf("%d-Sided One Sample Student's t-test", tail), tab, sprintf("%1.2f%% Confidence Interval: (%f, %f)", 100 * conf, ci[1], ci[2])))
}

two_sample_se <- function(s1, n1, s2, n2) {
  return(sqrt(((s1 ^ 2) / n1) + ((s2 ^ 2) / n2)))
}

two_sample_t <- function(x1, s1, n1, x2, s2, n2, tail, conf = 0.95) {
  df = min(c(n1 - 1, n2 - 1))
  se = two_sample_se(s1, n1, s2, n2)
  t = ((x1 - x2) - 0) / (se)
  p = t_test(t, df, tail)
  ci = ci_interval(abs(x1 - x2), t_crit(conf, df) * se)
  tab = as.data.frame(matrix(c(df, se, t, p, p_evidence(p)), ncol = 5, dimnames = list(c(sprintf("%d-side", tail)), c('df', 'se(x1 - x2)', 't_stat', 'p_value', 'evidence'))))

  print(list(sprintf("%d-Sided Two Sample t-test SD's not equal", tail), tab, sprintf("%1.2f%% Confidence Interval: (%f, %f)", 100 * conf, ci[1], ci[2])))
}

pooled_S2p <- function(s1, n1, s2, n2) {
  numerator = ((n1 - 1) * (s1 ^ 2)) + ((n2 - 1) * (s2 ^ 2))
  denominator = (n1 - 1) + (n2 - 1)
  return(numerator / denominator)
}

pooled_se <- function(s1, n1, s2, n2) {
  S2p = pooled_S2p(s1, n1, s2, n2)
  return(sqrt(S2p * ((1 / n1) + (1 / n2))))
}

pooled_t <- function(x1, s1, n1, x2, s2, n2, tail, conf = 0.95) {
  df = n1 + n2 - 2
  S2p = pooled_S2p(s1, n2, s2, n2)
  se = pooled_se(s1, n1, s2, n2)
  t = ((x1 - x2) - 0) / (se)
  p = t_test(t, df, tail)
  ci = ci_interval(abs(x1 - x2), t_crit(conf, df) * se)
  tab = as.data.frame(matrix(c(df, S2p, se, t, p, p_evidence(p)), ncol = 6, dimnames = list(c(sprintf("%d-side", tail)), c('df', 'S2p', 'se(x1 - x2)', 't_stat', 'p_value', 'evidence'))))

  print(list(sprintf("%d-Sided Pooled t-test SD's equal", tail), tab, sprintf("%1.2f%% Confidence Interval: (%f, %f)", 100 * conf, ci[1], ci[2])))
}


two_proportions_se <- function(phat1, n1, phat2, n2) {
  left = (phat1 * (1 - phat1)) / n1
  right = (phat2 * (1 - phat2)) / n2
  return(sqrt(left + right))
}

two_proportions_z <- function(phat1, n1, phat2, n2, tail, conf = 0.95) {
  se = two_proportions_se(phat1, n1, phat2, n2)
  z = ((phat1 - phat2) - 0) / se
  p = z_test(z, tail)
  ci = ci_interval(abs(phat1 - phat2), z_crit(conf) * se)
  tab = as.data.frame(matrix(c(se, z, p, p_evidence(p)), ncol = 4, dimnames = list(c(sprintf("%d-side", tail)), c('se(ph1 - ph2)', 'z_stat', 'p_value', 'evidence'))))

  print(list(sprintf("%d-Sided Proportion z-test", tail), tab, sprintf("%1.2f%% Confidence Interval: (%f, %f)", 100 * conf, ci[1], ci[2])))
}


correlation_se <- function(r, n) {
  return(sqrt((1 - (r ^ 2)) / (n - 2)))
}

correlation_t <- function(r, n, tail, p = 0, conf = 0.95) {
  df = n - 2
  se = correlation_se(r, n)
  t = (r - p) / se
  p = t_test(t, df, tail)
  # TODO: Ask about CI not giving right result
  ci = ci_interval(r, t_crit(conf, df) * se)
  tab = as.data.frame(matrix(c(df, se, t, p, p_evidence(p)), ncol = 5, dimnames = list(c(sprintf("%d-side", tail)), c('df', 'se(r)', 't_stat', 'p_value', 'evidence'))))

  print(list(sprintf("%d-Sided Correlation t-test", tail), tab, sprintf("%1.2f%% Confidence Interval: (%f, %f)", 100 * conf, ci[1], ci[2])))
}
