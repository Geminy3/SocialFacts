
# SocialFacts

<!-- badges: start -->
<!-- badges: end -->

SocialFacts brings a tool to produce easily multiple indicators commonly used in social sciences : 
- Odds Ratio, with p.value and CI
- Average Marginal Effects, with CI
- A cramer's V table, to check correlation between your categorical variables
- A new tool called Average Attributable Fraction, described by Ferguson and al. here : 
We use the original `graphPAF` library, and facilitate it's use to produce a table which resume all of this tools in one place.

## Installation

You can install the development version of SocialFacts like so:

``` r
# install.packages("SocialFacts")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SocialFacts)
## basic example code

### Fit a logistic model
model <- glm(var_dep ~ some + var + you + have, data = data, family = binomial(...))

### You can check the correlation between your categorical data
get_cramer(var = c("var_1", "var_2"), metric = 'cramer', data = data)

### To compute and store the AAF
resAAF <- get_AAF(model = model, nvar = 3, vars_dep = "var_dep", data = data)

### To get the summary table
resume_tab <- result_tab(result_tab(model = model, var_ref = "var_dep", var_names = c("var_1", "var_2"),
                       res_AAF = resAAF$res, data = data)

### To view the table
resume_tab

### To export the table
resume_tab |>
  gtsave(filename = "~/the/path/to/store/it/TAB_RECAP.png", expand = 10)
```

