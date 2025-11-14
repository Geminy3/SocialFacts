library(openxlsx)
library(dplyr)
library(graphPAF) 
library(averisk) 
library(MASS) 
library(gtsummary)
library(gt)
library(ggplot2)
library(margins)
library(ggeffects)
library(modelbased)
library(rcompanion)
library(stringr)

# FONCTIONS

### LISTE
get_list <- function(name_var, type) {
  vec <- rep_len(list(), length(name_var))
  names(vec) = name_var
  for (v in name_var) {
    vec[[v]] <- rep_len(0, length(name_var))
    names(vec[[v]]) <- name_var
  }
  return(vec)
}

### GET CRAMER TAB

get_cramer_tab <- function(var, metric = 'cramer', data) {
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

### AAF

Get_AF <- function(model, nvar = NULL, name_var = NULL, vars_dep = NULL, data = NULL, 
                   nbootstrap = 50) { 
  # SETUP 
  
  if (!is.null(name_var)) { 
    print("Using named variable") 
    vars_vec = c(name_var, vars_dep) 
  } else { 
    print("Using random variable") 
    if (nvar <= ncol(model$model)) { 
      vars.ind <- names(model$model)[2:nvar] 
      vars.dep <- names(model$model)[1] 
      vars_vec <- c(vars.ind, vars.dep) 
    } else { 
      print("nvar is longer than the number of variable names\n Using max length") 
      vars.ind <- names(model$model)[2:ncol(model$model)] 
      vars.dep <- names(model$model)[1] 
      vars_vec <- c(vars.ind, vars.dep) 
    } 
  } 
  parent_vec <- vars_vec[1:length(vars_vec)-1] 
  node <- vars_vec[1:length(vars_vec)] 
  
  # COMPUTE 
  #print(vars_vec)
  #print(parent_vec)
  #print(node)
  print("COMPUTE")
  
  models <- automatic_fit( 
    data_clean(data, vars=vars_vec, model = model), 
    parent_list = c(replicate(n = length(parent_vec),  
                              c(),  
                              simplify = F), 
                    list(parent_vec)), 
    node_vec = node)
  
  res <- average_paf(
    data_clean(data, vars=vars_vec, model = model), 
    model_list = models,  
    parent_list = c(replicate(n = length(parent_vec),  
                              c(),  
                              simplify = F),  
                    list(parent_vec)), 
    node_vec = node,  
    prev=NULL,  
    exact = F,
    nperm = 1000,  
    boot_rep = nbootstrap, 
    ci = TRUE, #ci_type = 'bca',
    ci_level = 0.95,
    verbose = T) 
  
  return(res) 
}

### TAB RECAP

export_tab <- function(model, var_ref, res_AAF, data) {
  
  var = sym(var_ref)
  
  #ODDS RATIO TAB
  odds_tab <- tbl_regression(model, exponentiate = T)

  #AME TAB
  marge <- model %>%
    gtsummary::tbl_regression(
      tidy_fun = broom.helpers::tidy_avg_slopes,
      estimate_fun = scales::label_percent(
        accuracy = 0.1,
        style_positive = "plus"
      )
    )
  marge$table_body %>%
    mutate(label = str_remove(label, ' - .*$'), term = str_remove(label, ' - .*$')) -> marge$table_body 
  
  #CONTINGENCE TAB
  contingence <- data %>%
    tbl_summary(
      by = !!var_ref,
      include = c(-weights, -ski_alp, -REG)
    ) %>%
    add_ci() %>%
    add_p()
  
  # MERGE TAB
  clean_AAF <- res_AAF %>% filter(position %in% c("Average", "Joint")) %>%
    arrange(desc(est)) %>%
    gt::gt() %>%
    tab_header(title = "Average Attributable Fraction", 
               subtitle = "sur la pratique d'un sport en club") %>%
    gt::tab_spanner(label = "CI", columns = c("norm_lower", "norm_upper")) %>%
    gt::tab_spanner(label = "Estimation moyenne", columns = c("est", "debiased_est")) %>%
    gt::fmt_number(columns = everything(), suffixing = T) %>%
    gt::data_color(
      columns = c('debiased_est', 'norm_lower', 'norm_upper'),
      method = "numeric",
      rows = c(contains('Average', vars = c(position))),
      palette = c("gold", "darkgreen"),
    ) %>%
    gt::data_color(
      columns = everything(), #c("position", "debiased_est", "norm_lower", "norm_upper"),
      rows = c(contains('Joint', vars = c(position))),
      palette = c("lightblue"),
    ) %>%
    gt::tab_options(table.width = gt::pct(100))
  
  tab_recap <- tbl_merge(list(odds_tab, marge, contingence), tab_spanner = c("Odds ratio", 'Average Marginal Effects', var_ref)) %>%
    as_gt() %>%
    tab_header(title = "Tableau Récapitulatif", 
               subtitle = paste("Sur la variable", var_ref)) %>%
    data_color(
      columns = c("estimate_1", "ci_1", "estimate_2", "ci_2"),
      rows = everything(),
      palette = c("gold", "darkgreen"),
      na_color = "white"
    ) %>%
    data_color(
      columns = c("p.value_1", "p.value_2", 'p.value_3'),
      rows = everything(),
      palette = c("lightblue", "#F2715A"),
      na_color = "white"
    ) %>%
    gt::tab_footnote(footnote = "Source : ENPPS 2020")
  
  tab <- gt::tab_source_note(tab_recap, clean_AAF %>% gt::as_raw_html())
  return(tab)
}
