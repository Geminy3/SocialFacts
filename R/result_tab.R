globalVariables(c("label", "position", 'est'))

#' Get the result table of OR, AME and AAF
#'
#' @param model The model used to perform statistical analysis
#' @param var_ref The variable to explain in the statistical model
#' @param var_names A list with the name of the variable you want to show on the contingency tables
#' @param res_AAF the result of the AAF computation with `get_AAF()` function (done outside of this function because of the length of the computation)
#' @param data The data used for the statistical analysis
#'
#' @returns A gt table
#' @import gt
#' @import dplyr
#' @import gtsummary
#' @importFrom broom.helpers tidy_avg_slopes
#' @import scales
#' @import stringr
#' @export
#'
#' @examples
#' \dontrun{res_tab <- result_tab(model = glmmodel, var_ref = "var_0", var_names = c("var_1", "var_2", "var_3"),
#'                     res_AAF = resAAF$res, data = data)}
result_tab <- function(model = NULL, var_ref = NULL, var_names = NULL, res_AAF = NULL, data = data.frame()) {

  var = dplyr::sym(var_ref)

  #ODDS RATIO TAB
  odds_tab <- gtsummary::tbl_regression(model, exponentiate = T)

  #AME TAB
  marge <- model |>
    gtsummary::tbl_regression(
      tidy_fun = broom.helpers::tidy_avg_slopes,
      estimate_fun = scales::label_percent(
        accuracy = 0.1,
        style_positive = "plus"
      )
    )
  marge$table_body |>
    dplyr::mutate(label = stringr::str_remove(label, ' - .*$'), term = stringr::str_remove(label, ' - .*$')) -> marge$table_body

  #CONTINGENCE TAB
  contingence <- data |>
    gtsummary::tbl_summary(
      by = !!var_ref,
      include = var_names#c(-weights, -ski_alp, -REG)
    ) |>
    gtsummary::add_ci() |>
    gtsummary::add_p()

  # MERGE TAB
  clean_AAF <- res_AAF |> filter(position %in% c("Average", "Joint")) |>
    dplyr::arrange(desc(est)) |>
    gt::gt() |>
    gt::tab_header(title = "Average Attributable Fraction",
               subtitle = "sur la pratique d'un sport en club") |>
    gt::tab_spanner(label = "CI", columns = c("norm_lower", "norm_upper")) |>
    gt::tab_spanner(label = "Estimation moyenne", columns = c("est", "debiased_est")) |>
    gt::fmt_number(columns = dplyr::everything(), suffixing = T) |>
    gt::data_color(
      columns = c('debiased_est', 'norm_lower', 'norm_upper'),
      method = "numeric",
      rows = c(dplyr::contains('Average', vars = c(position))),
      palette = c("lightblue", "#F2715A"),
    ) |>
    gt::data_color(
      columns = dplyr::everything(), #c("position", "debiased_est", "norm_lower", "norm_upper"),
      rows = c(dplyr::contains('Joint', vars = c(position))),
      palette = c("gold"),
    ) |>
    gt::tab_options(table.width = gt::pct(100))

  tab_recap <- tbl_merge(list(odds_tab, marge, contingence), tab_spanner = c("Odds ratio", 'Average Marginal Effects', var_ref)) |>
    gtsummary::as_gt() |>
    gt::tab_header(title = "Tableau Recapitulatif",
               subtitle = paste("Sur la variable", var_ref)) |>
    gt::data_color(
      columns = c("estimate_1", "ci_1", "estimate_2", "ci_2"),
      rows = dplyr::everything(),
      palette = c("lightblue", "#F2715A"),
      na_color = "white"
    ) |>
    gt::data_color(
      columns = c("p.value_1", "p.value_2", 'p.value_3'),
      rows = dplyr::everything(),
      palette = c("gold", "darkgreen"),
      na_color = "white"
    ) |>
    gt::tab_footnote(footnote = "Source : ENPPS 2020")

  tab <- gt::tab_source_note(tab_recap, clean_AAF |> gt::as_raw_html())
  return(tab)
}
