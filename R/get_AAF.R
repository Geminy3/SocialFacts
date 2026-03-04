#' @title get_AAF
#' @description
#' Get the AAF
#'
#' @param model The model you want to compute AAF on
#' @param nvar You can choose this `integer` parameter if you prefer to add explanatory variables in the order they appear in the model. You don't have to use `name_var` parameter.
#' @param name_var A list of explanatory variables you want to compute AAF on
#' @param vars_dep A string with the name of the variable you want to explain
#' @param data The data you used to fit the model (with the variable in name_var and vars_dep)
#' @param nbootstrap An integer with the n bootsrap replication you want. By default, 50.
#' @param nperm Number of permutation between modalities and variable to compute the AAF
#'
#' @returns A sf table with the result of the AAF computation
#' @import averisk
#' @import graphPAF
#' @export
#'
#' @examples
#' set.seed(9876)
#' data <- data.frame(
#'    "var_dep" = as.factor(sample(c(rep_len(c(0, 1), length.out = 1000)), size = 100)),
#'    "var_1" = as.factor(sample(c(rep_len(c(1, 2, 3), length.out = 1000)), size = 100)),
#'    "var_2" = as.factor(sample(c(rep_len(c(3, 1, 2), length.out = 100)), size = 100))
#'    )
#' glmmodel <- glm(var_dep ~ var_1 + var_2,
#'                 data = data, family = binomial("logit"))
#' res <- get_AAF(model = glmmodel, nvar = 3, vars_dep = "var_dep", data = data,
#'                nbootstrap = 5)
#' #res <- get_AAF(model = glmmodel,
#' #               name_var = c("var_1", "var_2"),
#' #              vars_dep = "var_dep", data = data, nbootstrap = 5)
get_AAF <- function(model = NULL, nvar = NULL, name_var = NULL, vars_dep = NULL, data = NULL,
                   nbootstrap = 50, nperm = 100) {
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

  models <- graphPAF::automatic_fit(
    graphPAF::data_clean(data, vars=vars_vec, model = model),
    parent_list = c(replicate(n = length(parent_vec),
                              c(),
                              simplify = F),
                    list(parent_vec)),
    node_vec = node)

  res <- graphPAF::average_paf(
    graphPAF::data_clean(data, vars=vars_vec, model = model),
    model_list = models,
    parent_list = c(replicate(n = length(parent_vec),
                              c(),
                              simplify = F),
                    list(parent_vec)),
    node_vec = node,
    prev=NULL,
    exact = F,
    nperm = nperm,
    boot_rep = nbootstrap,
    ci = TRUE, #ci_type = 'bca',
    ci_level = 0.95,
    verbose = T)

  return(res)
}
