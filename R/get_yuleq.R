#' @title Get Yule's Q
#'
#' @description
#' Get Yule's Q
#'
#' @param vars_dep A string with name of the dependant variable to use as one variable for the 2x2 table
#' @param name_var A vector of explanatory variable you want to compute Yule's Q on
#' @param data A dataframe with the variable named before
#' @param source A string with the name of the data source
#' @param alpha A integer with the alpha value for fisher.test and IC
#'
#' @returns A gt table with the Yule's computed for the variable in name_var
#' @import gt
#' @importFrom psych dummy.code
#' @importFrom stats fisher.test
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' \dontrun{get_yuleq(vars_dep = "dependant_var", name_var = c("var_1", "var_2"),
#'                     data = data, source = "dataSource - Year", alpha = 0.01)}
get_yuleq <- function(vars_dep = NULL, name_var = c(), data = NULL, source = "", alpha = 0.01) {

  if (is.null(vars_dep)) {
    return("No dependant variable")
  }
  if (length(name_var) < 1) {
    return("No variable to compute Yule's Q on")
  }
  if (is.null(data)) {
    return("No Data !")
  }
  vars <- c()
  modas <- c()
  yules <- c()
  se.Qs <- c()
  ll.Qs <- c()
  ul.Qs <- c()
  z <- qnorm(1 - alpha/2)
  fisher <- c()
  for (var in name_var) {
    if (var != vars_dep) {
      if (length(unique(data[[var]])) > 2) {
        dum_tab <- psych::dummy.code(data[[var]])
        #print(colnames(dum_tab))
        for (col in colnames(dum_tab)) {
          #print(col)
          #print(dum_tab[,col])
          t <- table(dum_tab[,col], data[[vars_dep]])
          #print(t)
          pair_conco = t[1] * t[4]
          pair_disco = t[2] * t[3]
          Yule = (pair_conco - pair_disco) / (pair_conco + pair_disco)

          or <- (t[4] + 0.5) * (t[1] + 0.5)/((t[2] + 0.5) * (t[3] + 0.5))
          se.lor <- sqrt(1/(t[1] + 0.5) + 1/(t[2] + 0.5) + 1/(t[3] + 0.5) +
                           1/(t[4] + 0.5))
          ll.or <- exp(log(or) - z * se.lor)
          ul.or <- exp(log(or) + z * se.lor)
          Q <- (or - 1)/(or + 1)
          se.Q <- 0.5 * (1 - Q^2) * se.lor
          ll.Q <- (ll.or - 1)/(ll.or + 1)
          ul.Q <- (ul.or - 1)/(ul.or + 1)

          vars <- c(vars, var)
          modas <- c(modas, col)
          yules <- c(yules, Yule)
          #Qs <- c(Qs, Q)
          se.Qs <- c(se.Qs, se.Q)
          ll.Qs <- c(ll.Qs, ll.Q)
          ul.Qs <- c(ul.Qs, ul.Q)
          fisher <- c(fisher, stats::fisher.test(dum_tab[,col], data[[vars_dep]], conf.level = 1-alpha)$p.value)
        }
      } else {
        #print(var)
        t <- table(data[[var]], data[[vars_dep]])
        #print(t)
        ref <- levels(data[,var])[1]
        col <- levels(data[,var])[2]
        pair_conco = t[1] * t[4]
        pair_disco = t[2] * t[3]
        Yule = (pair_conco - pair_disco) / (pair_conco + pair_disco)
        #print(Yule)
        vars <- c(vars, var)
        modas <- c(modas, ref)
        yules <- c(yules, NA)
        #Qs <- c(Qs, NA)
        se.Qs <- c(se.Qs, NA)
        ll.Qs <- c(ll.Qs, NA)
        ul.Qs <- c(ul.Qs, NA)
        fisher <- c(fisher, NA)
        vars <- c(vars, var)
        modas <- c(modas, col)
        yules <- c(yules, Yule)
        #Qs <- c(Qs, Q)
        se.Qs <- c(se.Qs, se.Q)
        ll.Qs <- c(ll.Qs, ll.Q)
        ul.Qs <- c(ul.Qs, ul.Q)
        fisher <- c(fisher, stats::fisher.test(data[,var], data[[vars_dep]], conf.level = 1-alpha)$p.value)
      }
    }
  }
  YuleQ <- list("var" = vars, "moda" = modas, "YuleQ" = yules,
                "se.Q" = se.Qs, "ll" = ll.Qs, "ul" = ul.Qs, "p.value" = fisher)
  # print(YuleQ)
  YuleQ <- YuleQ |>
    as.data.frame() |>
    gt(groupname_col = "var", row_group_as_column = T) |>
    gt::sub_missing(columns = YuleQ, missing_text = "- Ref -") |>
    gt::sub_missing(columns = c(se.Q, ll, ul, p.value), missing_text = "") |>
    gt::data_color(
      columns = YuleQ,
      method = "bin",
      palette = c("#F44333", "#2BBFFF", "#F44333"), #"#81F",
      bins = c(-1, -0.2, -0.1, 0.1, 0.2, 1),
      na_color = "white"
    ) |>
    gt::fmt_number(columns = everything(), decimals = 3) |>
    gt::fmt_percent(columns = c(YuleQ, ll, ul), decimals = 2) |>
    gt::data_color(
      columns = p.value,
      palette = c("gold", "darkgreen"), #"#81F",
      na_color = "white"
    ) |>
    gt::cols_label(
      moda = "",
      YuleQ = "Q de Yule"
    ) |>
    gt::tab_header(
      title = "Q de Yule",
      subtitle = paste("Sur la variable", vars_dep)
    ) |>
    gt::tab_footnote(
      footnote = paste("Source :", source)
    )
  return(YuleQ)
}
