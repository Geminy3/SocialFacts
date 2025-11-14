#' Get a list with the names of the variables pass as parameter
#'
#' @param name_var A list of variable you want to pass a name for a list
#'
#' @returns a list of zero named after the variable in name_var parameter
#' @export
#'
#' @examples
#' vec <- get_list(name_var = c("var1", "var2"))
get_list <- function(name_var = NULL) {
  vec <- rep_len(list(), length(name_var))
  names(vec) = name_var
  for (v in name_var) {
    vec[[v]] <- rep_len(0, length(name_var))
    names(vec[[v]]) <- name_var
  }
  return(vec)
}


#' Get Cramer of YuleQ's tab
#'
#' @param var A list of variable name you want to compute "metric" on
#' @param metric Choose between 'Cramer' or 'YuleQ' parameter to select one of this metrics
#' @param df The data table you want to compute 'metric' parameter on
#'
#' @returns A list of gt information
#' @importFrom rcompanion cramerV
#' @import gt
#' @importFrom stats cor
#' @importFrom stats chisq.test
#' @export
#'
#' @examples
#' cramer_tab(c("var_1", "var_2"), metric = 'cramer', df = df)
cramer_tab <- function(var = NULL, metric = 'cramer', df = NULL) {
  vec_metric <- get_list(var)
  vec_chi <- get_list(var)
  if (length(var) == 0) {
    return("No variable selected")
  }
  if (typeof(df) != "list") {
    return("Data is not a dataframe !")
  }
  for (x in var) {
    for (y in var) {
      if (x != y) {
        #print(paste(x, "-" ,y))
        if (metric == 'cramer') {
          vec_metric[[x]][y] <- rcompanion::cramerV(df[[x]], df[[y]])[['Cramer V']]
          vec_chi[[x]][y] <- round(chisq.test(df[[x]], df[[y]])$p.value, 5)
        } else if (metric == 'yuleQ') {
          print('no yule')
        } else if (metric == 'cor') {
          vec_metric[[x]][y] <- stats::cor(df[[x]], df[[y]])
        }
      } else {
        vec_metric[[x]][y] <- NA
      }
    }
  }
  chi2 = as.data.frame(vec_chi) |>
    gt::gt(rownames_to_stub = T) |>
    gt::data_color(
      method = 'numeric',
      palette = c("purple", "gold"),
      domain = c(0,0.05), na_color = "white"
    ) |>
    gt::tab_header(title = "Chi2 des variables", subtitle = gt::md("arrondi - $10^{-5}$"))
  metric = as.data.frame(vec_metric) |>
    gt::gt(rownames_to_stub = T) |>
    gt::data_color(
      method = 'numeric',
      palette = c("purple", "gold"),
      domain = c(0,1)
    ) |>
    gt::tab_header(title = "Correlation des variables", subtitle = metric)
  return(list(chi2 = chi2, metric = metric))
}
