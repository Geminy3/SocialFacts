result_tab <- function(model, var_ref, res_AAF, data) {

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
      palette = c("lightblue", "#F2715A"),
    ) %>%
    gt::data_color(
      columns = everything(), #c("position", "debiased_est", "norm_lower", "norm_upper"),
      rows = c(contains('Joint', vars = c(position))),
      palette = c("gold"),
    ) %>%
    gt::tab_options(table.width = gt::pct(100))

  tab_recap <- tbl_merge(list(odds_tab, marge, contingence), tab_spanner = c("Odds ratio", 'Average Marginal Effects', var_ref)) %>%
    as_gt() %>%
    tab_header(title = "Tableau Récapitulatif",
               subtitle = paste("Sur la variable", var_ref)) %>%
    data_color(
      columns = c("estimate_1", "ci_1", "estimate_2", "ci_2"),
      rows = everything(),
      palette = c("lightblue", "#F2715A"),
      na_color = "white"
    ) %>%
    data_color(
      columns = c("p.value_1", "p.value_2", 'p.value_3'),
      rows = everything(),
      palette = c("gold", "darkgreen"),
      na_color = "white"
    ) %>%
    gt::tab_footnote(footnote = "Source : ENPPS 2020")

  tab <- gt::tab_source_note(tab_recap, clean_AAF %>% gt::as_raw_html())
  return(tab)
}
