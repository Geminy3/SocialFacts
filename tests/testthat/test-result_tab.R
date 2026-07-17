test_that("result_tab works", {
  data <- data.frame(
    "var_dep" = as.factor(sample(
      c(rep_len(c(0, 1), length.out = 1000)),
      size = 100
    )),
    "var_1" = as.factor(sample(
      c(rep_len(c(1, 2, 3), length.out = 1000)),
      size = 100
    )),
    "var_2" = as.factor(sample(
      c(rep_len(c(3, 1, 2), length.out = 100)),
      size = 100
    ))
  )
  glmmodel <- glm(
    var_dep ~ var_1 + var_2,
    data = data,
    family = binomial("logit")
  )

  res <- get_AAF(
    model = glmmodel,
    nvar = 3,
    vars_dep = "var_dep",
    name_var = c("var_1", "var_2"),
    data = data,
    nbootstrap = 2
  )
  expect_no_failure(
    res_tab <- result_tab(
      model = glmmodel,
      var_ref = "var_dep",
      var_names = c("var_1", "var_2"),
      res_AAF = res$res,
      source = "TESTDATA",
      data = data
    )
  )
})
