# get_AAF

Get the AAF

## Usage

``` r
get_AAF(
  model = NULL,
  nvar = NULL,
  name_var = NULL,
  vars_dep = NULL,
  data = NULL,
  nbootstrap = 50,
  nperm = 100
)
```

## Arguments

- model:

  The model you want to compute AAF on

- nvar:

  You can choose this `integer` parameter if you prefer to add
  explanatory variables in the order they appear in the model. You don't
  have to use `name_var` parameter.

- name_var:

  A list of explanatory variables you want to compute AAF on

- vars_dep:

  A string with the name of the variable you want to explain

- data:

  The data you used to fit the model (with the variable in name_var and
  vars_dep)

- nbootstrap:

  An integer with the n bootsrap replication you want. By default, 50.

- nperm:

  Number of permutation between modalities and variable to compute the
  AAF

## Value

A sf table with the result of the AAF computation

## Examples

``` r
set.seed(9876)
data <- data.frame(
   "var_dep" = as.factor(sample(c(rep_len(c(0, 1), length.out = 1000)), size = 100)),
   "var_1" = as.factor(sample(c(rep_len(c(1, 2, 3), length.out = 1000)), size = 100)),
   "var_2" = as.factor(sample(c(rep_len(c(3, 1, 2), length.out = 100)), size = 100))
   )
glmmodel <- glm(var_dep ~ var_1 + var_2,
                data = data, family = binomial("logit"))
res <- get_AAF(model = glmmodel, nvar = 3, vars_dep = "var_dep", data = data,
               nbootstrap = 5)
#> [1] "Using random variable"
#> [1] "COMPUTE"
#res <- get_AAF(model = glmmodel,
#               name_var = c("var_1", "var_2"),
#              vars_dep = "var_dep", data = data, nbootstrap = 5)
```
