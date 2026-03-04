# Get Yule's Q

Get Yule's Q

## Usage

``` r
get_yuleq(
  vars_dep = NULL,
  name_var = c(),
  data = NULL,
  source = "",
  alpha = 0.01,
  weight = NULL
)
```

## Arguments

- vars_dep:

  A string with name of the dependant variable to use as one variable
  for the 2x2 table

- name_var:

  A vector of explanatory variable you want to compute Yule's Q on

- data:

  A dataframe with the variable named before

- source:

  A string with the name of the data source

- alpha:

  A integer with the alpha value for fisher.test and IC

- weight:

  A vector with weigths corresponding to the dataframe individuals
  weights

## Value

A gt table with the Yule's computed for the variable in name_var

## Examples

``` r
data <- graphPAF::Hordaland_data
get_yuleq(vars_dep = "y", name_var = c("y",  "urban.rural"),
                    data = data, source = "dataSource - Year",
                    alpha = 0.01)


  


Q de Yule
```

Sur la variable y

Q de Yule

se.Q

ll

ul

p.value

urban.rural

0

\- Ref -

  

  

  

  

1

24.32%

0.019

19.33%

29.18%

0.000

Source : dataSource - Year
