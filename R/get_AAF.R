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
