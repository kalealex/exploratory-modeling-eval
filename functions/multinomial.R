multinomial_model_check <- function(spec, data) {
  library(tidyverse)
  library(brms)
  library(tidybayes)
  
  n_draws <- 10
  
  # catch values of negative inf on log transform
  log_trans_vars_mu <- str_match_all(spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  for (var_name in log_trans_vars_mu) {
    # compute log transform of variable and add to dataframe
    var <- sym(var_name)
    data <- data %>%
      mutate(
        "{{var}}" := if_else({{var}}==0.0,
                             0.001, # avoid -inf errors by fudging the zeros a bit
                             {{var}}
        ),
        "log_{{var}}" := log({{var}})
      )
    # replace log({{var}}) with log_{{var}} in spec
    spec <- str_replace_all(spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  }
  
  # fit model
  spec <- as.formula(spec)
  model <- eval(bquote(brm(
    formula = bf(.(spec)),
    family = multinomial(),
    chains = 2,
    cores = 2,
    iter = 1000,
    warmup = 500,
    data = data
  )))
  
  # get predictive distribution (look how much easier this is with a Bayesian model)
  output <- data %>%
    add_predicted_draws(model, seed = 14, n = n_draws)
}