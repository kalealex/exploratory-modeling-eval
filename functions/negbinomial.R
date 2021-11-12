negbinomial_model_check <- function(mu_spec, sigma_spec = "~1", data) {
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
  log_trans_vars_sigma <- str_match_all(sigma_spec, "log\\(\\s*(.*?)\\s*\\)")[[1]][,2]
  for (var_name in log_trans_vars_sigma) {
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
    # replace log({{var}}) with log_{{var}} in sigma_spec
    sigma_spec <- str_replace_all(sigma_spec, paste("log\\(", var_name, "\\)", sep = ""), paste("log_", var_name, sep = ""))
  }
  
  # fit model
  mu_spec <- as.formula(mu_spec)
  sigma_spec <- as.formula(sigma_spec)
  model <- eval(bquote(gamlss(.(mu_spec), sigma.fo = .(sigma_spec), data = data, family = NBII)))
  
  # get summary statistics describing model predictions
  pred.mu <- predict(model, se.fit = TRUE)
  pred.sigma <- predict(model, what = "sigma", se.fit = TRUE)
  output <- data %>%
    mutate(
      logmu.expectation = pred.mu$fit,                    # add fitted logmu and standard errors to dataframe
      logmu.se = pred.mu$se.fit,
      df = df.residual(model),                            # get degrees of freedom
      logsigma.expectation = pred.sigma$fit,              # add fitted logsigma and standard errors to dataframe 
      logsigma.se = pred.sigma$se.fit
    )
  
  # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
  output <- output %>%
    mutate(
      .draw = list(1:n_draws),                            # generate list of draw numbers
      t1 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into log mu
      t2 = map(df, ~rt(n_draws, .)),                      # simulate draws from t distribution to transform into log sigma
    ) %>%
    unnest(cols = c(".draw", "t1", "t2")) %>%
    mutate(
      logmu = t1 * logmu.se + logmu.expectation,          # scale and shift t to get a sampling distribution of log mu
      logsigma = t2 * logsigma.se + logsigma.expectation, # scale and shift t to get a sampling distribution of log sigma
      mu = exp(logmu),                                    # backtransform to sampling distributions of mu and sigma parameters
      sigma = exp(logsigma)
    ) %>%
    rowwise() %>%
    mutate(
      prediction = rNBII(1, mu, sigma)                    # compute predictive distribution of counts
    )
}