test_that("get_yuleq works", {
  set.seed(9876)

  df <- data.frame(
    "var_dep" = as.factor(sample(c(rep_len(c(0, 1), length.out = 1000)), size = 100)),
    "var_1" = as.factor(sample(c(rep_len(c(1, 2, 3), length.out = 1000)), size = 100)),
    "var_2" = as.factor(sample(c(rep_len(c(3, 1, 2), length.out = 100)), size = 100))
  )

  expect_no_failure(
    res <- get_yuleq(vars_dep = "var_dep", name_var = c("var_1", "var_2"), data = df, source = "TEST")
    )
})
