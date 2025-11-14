test_that("get_AAF works", {

  set.seed(9876)

  df <- data.frame(
    "var_dep" = as.factor(sample(c(rep_len(c(0, 1), length.out = 1000)), size = 100)),
    "var_1" = as.factor(sample(c(rep_len(c(1, 2, 3), length.out = 1000)), size = 100)),
    "var_2" = as.factor(sample(c(rep_len(c(3, 1, 2), length.out = 100)), size = 100))
  )
  #print(str(df))
  model <- glm(var_dep ~ var_1 + var_2, data = df, family = binomial(link="logit"))
  expect_no_failure(get_AAF(model = model, nvar = 3, vars_dep = "var_dep", data = df, nbootstrap = 5))
  #print(res)
})
