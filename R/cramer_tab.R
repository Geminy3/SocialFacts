get_list <- function(name_var, type) {
  vec <- rep_len(list(), length(name_var))
  names(vec) = name_var
  for (v in name_var) {
    vec[[v]] <- rep_len(0, length(name_var))
    names(vec[[v]]) <- name_var
  }
  return(vec)
}

cramer_tab <- function(var, metric = 'cramer', data) {
  vec_metric <- get_list(var, 'list')
  vec_chi <- get_list(var, 'list')
  for (x in var) {
    for (y in var) {
      if (x != y) {
        #print(paste(x, "-" ,y))
        if (metric == 'cramer') {
          vec_metric[[x]][y] <- cramerV(data[[x]], data[[y]])[['Cramer V']]
          vec_chi[[x]][y] <- round(chisq.test(data[[x]], data[[y]])$p.value, 5)
        } else if (metric == 'yuleQ') {
          print('no yule')
        } else if (metric == 'cor') {
          vec_metric[[x]][y] <- cor(data[[x]], data[[y]])
        }
      } else {
        vec_metric[[x]][y] <- NA
      }
    }
  }
  chi2 = as.data.frame(vec_chi) %>%
    gt::gt(rownames_to_stub = T) %>%
    gt::data_color(
      method = 'numeric',
      palette = c("purple", "gold"),
      domain = c(0,0.05), na_color = "white"
    ) %>%
    tab_header(title = "Chi2 des variables", subtitle = gt::md("arrondi à $10^{-5}$"))
  metric = as.data.frame(vec_metric) %>%
    gt::gt(rownames_to_stub = T) %>%
    gt::data_color(
      method = 'numeric',
      palette = c("purple", "gold"),
      domain = c(0,1)
    ) %>%
    tab_header(title = "Corrélation des variables", subtitle = metric)
  return(list(chi2 = chi2, metric = metric))
}
