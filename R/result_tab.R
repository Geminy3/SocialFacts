globalVariables(c("label", "position", 'est'))

#' @title result_tab
#' @description
#' Get the result table of OR, AME and AAF
#'
#' @param model The model used to perform statistical analysis
#' @param var_ref The variable to explain in the statistical model
#' @param var_names A list with the name of the variable you want to show on the contingency tables
#' @param res_AAF the result of the AAF computation with `get_AAF()` function (done outside of this function because of the length of the computation)
#' @param data The data used for the statistical analysis
#' @param source Data source information to add to the plots
#'
#' @returns A gt table
#' @import gt
#' @import dplyr
#' @importFrom gtsummary tbl_regression
#' @importFrom gtsummary tbl_summary
#' @importFrom gtsummary add_ci
#' @importFrom gtsummary add_p
#' @importFrom gtsummary tbl_merge
#' @import broom.helpers
#' @import scales
#' @import stringr
#' @import graphPAF
#' @importFrom marginaleffects avg_slopes
#' @export
#'
#' @examples
#' set.seed(9876)
#' data <- data.frame(
#'    "var_dep" = as.factor(sample(c(rep_len(c(0, 1), length.out = 1000)), size = 100)),
#'    "var_1" = as.factor(sample(c(rep_len(c(1, 2, 3), length.out = 1000)), size = 100)),
#'    "var_2" = as.factor(sample(c(rep_len(c(3, 1, 2), length.out = 100)), size = 100))
#'    )
#' glmmodel <- glm(var_dep ~ var_1 + var_2, data = data, family = binomial("logit"))
#' res <- get_AAF(model = glmmodel, nvar = 3, vars_dep = "var_dep", data = data,
#'                nbootstrap = 5)
#'
#' \dontrun{res_tab <- result_tab(model = glmmodel, var_ref = "var_dep",
#'                       var_names = c("var_1", "var_2"),
#'                       res_AAF = resAAF$res, source = "TESTDATA", data = data)}
result_tab <- function(model = NULL, var_ref = NULL, var_names = NULL,
                       res_AAF = NULL, source = "", data = data.frame()) {

  var = dplyr::sym(var_ref)

  #ODDS RATIO TAB
  odds_tab <- gtsummary::tbl_regression(model, exponentiate = T)

  #AME TAB
  ## You have to call the marginaleffects function somehow so it don't segfault
  marge <- marginaleffects::avg_slopes(model)
  #broom.helpers::tidy_avg_slopes(model)
  ##
  marge <- model |>
    gtsummary::tbl_regression(
      tidy_fun = broom.helpers::tidy_avg_slopes,
      estimate_fun = scales::label_percent(
        accuracy = 0.1,
        style_positive = "plus"
      )
    )
  marge$table_body |>
    dplyr::mutate(
      label = stringr::str_remove(label, ' - .*$'),
      term = stringr::str_remove(label, ' - .*$')
      ) -> marge$table_body

  #CONTINGENCE TAB
  contingence <- data |>
    gtsummary::tbl_summary(
      by = !!var_ref,
      include = var_names#c(-weights, -ski_alp, -REG)
    ) |>
    gtsummary::add_ci() |>
    gtsummary::add_p()

  # MERGE TAB
  clean_AAF <- res_AAF |> dplyr::filter(position %in% c("Average", "Joint")) |>
    dplyr::arrange(desc(est)) |>
    gt::gt() |>
    gt::tab_header(title = "Average Attributable Fraction",
               subtitle = paste("Sur la variable", var_ref)) |>
    gt::tab_spanner(label = "CI", columns = c("norm_lower", "norm_upper")) |>
    gt::tab_spanner(label = "Estimation moyenne", columns = c("est", "debiased_est")) |>
    gt::fmt_number(columns = dplyr::everything(), suffixing = T) |>
    gt::data_color(
      columns = c('debiased_est', 'norm_lower', 'norm_upper'),
      method = "numeric",
      rows = c(dplyr::contains('Average', vars = dplyr::any_of(c(position)))),
      palette = c("lightblue", "#F2715A"),
    ) |>
    gt::data_color(
      columns = dplyr::everything(), #c("position", "debiased_est", "norm_lower", "norm_upper"),
      rows = c(dplyr::contains('Joint', vars = dplyr::any_of(c(position)))),
      palette = c("gold"),
    ) |>
    gt::tab_options(table.width = gt::pct(100))

  tab_recap <- gtsummary::tbl_merge(list(odds_tab, marge, contingence), tab_spanner = c("Odds ratio", 'Average Marginal Effects', var_ref)) |>
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
    gt::tab_footnote(footnote = paste("Source :", source))

  tab <- gt::tab_source_note(tab_recap, clean_AAF |> gt::as_raw_html())
  return(tab)
}
