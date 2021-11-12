poisson_model_check <- function(mu_spec, sigma_spec = "~1", data) {
  library(tidyverse)
  library(gamlss)
  
  # settings
  n_draws <- 10
  
  # catch values of negative inf on log transform
  log_trans_vars_mu <- str_match_all(mu_spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
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
    # replace log({{var}}) with log_{{var}} in mu_spec
    mu_spec <- str_replace_all(mu_spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  }
  
  # fit model
  mu_spec <- as.formula(mu_spec)
  model <- eval(bquote(gamlss(.(mu_spec), data = data, family = PO)))
  
  # get summary statistics describing model predictions
  pred <- predict(model, se.fit = TRUE)
  output <- data %>%
    mutate(
      logmu.expectation = pred$fit,                       # add fitted predictions and standard errors to dataframe
      logse.expectation = pred$se.fit,
      df = df.residual(model)                             # get degrees of freedom
    )
  
  # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
  output <- output %>%
    mutate(
      .draw = list(1:n_draws),                            # generate list of draw numbers
      t = map(df, ~rt(n_draws, .))                        # simulate draws from t distribution to transform into log mean rates
    ) %>%
    unnest(cols = c(".draw", "t")) %>%
    mutate(
      logmu = t * logse.expectation + logmu.expectation,  # scale and shift t to get a sampling distribution of log mean rates
      mu = exp(logmu)                                     # backtransform to sampling distribution of mean rate parameter
    ) %>%
    rowwise() %>%
    mutate(
      prediction = rpois(1, mu)                           # compute predictive distribution of counts
    )
}