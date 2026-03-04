# result_tab

Get the result table of OR, AME and AAF

## Usage

``` r
result_tab(
  model = NULL,
  var_ref = NULL,
  var_names = NULL,
  res_AAF = NULL,
  source = "",
  data = data.frame()
)
```

## Arguments

- model:

  The model used to perform statistical analysis

- var_ref:

  The variable to explain in the statistical model

- var_names:

  A list with the name of the variable you want to show on the
  contingency tables

- res_AAF:

  the result of the AAF computation with
  [`get_AAF()`](https://geminy3.github.io/SocialFacts/reference/get_AAF.md)
  function (done outside of this function because of the length of the
  computation)

- source:

  Data source information to add to the plots

- data:

  The data used for the statistical analysis

## Value

A gt table

## Examples

``` r
set.seed(9876)
data <- data.frame(
   "var_dep" = as.factor(sample(c(rep_len(c(0, 1), length.out = 1000)), size = 100)),
   "var_1" = as.factor(sample(c(rep_len(c(1, 2, 3), length.out = 1000)), size = 100)),
   "var_2" = as.factor(sample(c(rep_len(c(3, 1, 2), length.out = 100)), size = 100))
   )
glmmodel <- glm(var_dep ~ var_1 + var_2, data = data, family = binomial("logit"))
res <- get_AAF(model = glmmodel, nvar = 3, vars_dep = "var_dep", data = data,
               nbootstrap = 5)
#> [1] "Using random variable"
#> [1] "COMPUTE"

if (FALSE) res_tab <- result_tab(model = glmmodel, var_ref = "var_dep",
                      var_names = c("var_1", "var_2"),
                      res_AAF = resAAF$res, source = "TESTDATA", data = data) # \dontrun{}
```
