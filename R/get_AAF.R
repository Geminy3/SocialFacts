#' Get the AAF
#'
#' @param model The model you want to compute AAF on
#' @param nvar You can choose this `integer` parameter if you prefer to add explanatory variables in the order they appear in the model. You don't have to use `name_var` parameter.
#' @param name_var A list of explanatory variables you want to compute AAF on
#' @param vars_dep A string with the name of the variable you want to explain
#' @param data The data you used to fit the model (with the variable in name_var and vars_dep)
#' @param nbootstrap An integer with the n bootsrap replication you want. By default, 50.
#'
#' @returns A sf table with the result of the AAF computation
#' @export
#'
#' @examples
#' res <- get_AAF(model, nvar = 5, vars_dep = "dependant_var", data = data)
#' res <- get_AAF(model, name_var = c(var_1, var_2, var_3, var_4, var_5), vars_dep= "dependant_var", data = data, nbootstrap = 100)
Get_AAF <- function(model, nvar = NULL, name_var = NULL, vars_dep = NULL, data = NULL,
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
