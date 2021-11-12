normal_model_check <- function(mu_spec, sigma_spec = "~1", data) {
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
  head(data)
  
  # fit model
  mu_spec <- as.formula(mu_spec)
  sigma_spec <- as.formula(sigma_spec)
  model <- eval(bquote(gamlss(.(mu_spec), sigma.fo = .(sigma_spec), data = data)))
  
  # get summary statistics describing model predictions
  pred <- predict(model, se.fit = TRUE, type = "response")
  output <- data %>%
    mutate(
      mu.expectation = pred$fit,                          # add fitted predictions and standard errors to dataframe
      se.expectation = pred$se.fit,
      df = df.residual(model),                            # get degrees of freedom
      se.residual = sqrt(sum(residuals(model)^2) / df)    # get residual standard errors
    )
  
  # propagate uncertainty in fit to generate an ensemble of model predictions (mimic a posterior predictive distribution)
  output <- output %>%
    mutate(
      .draw = list(1:n_draws),                            # generate list of draw numbers
      t = map(df, ~rt(n_draws, .)),                       # simulate draws from t distribution to transform into means
      x = map(df, ~rchisq(n_draws, .))                    # simulate draws from chi-squared distribution to transform into sigmas
    ) %>%
    unnest(cols = c(".draw", "t", "x")) %>%
    mutate(
      mu = t * se.expectation + mu.expectation,           # scale and shift t to get a sampling distribution of means
      sigma = sqrt(df * se.residual^2 / x)                # scale and take inverse of x to get a sampling distribution of sigmas
    ) %>%
    rowwise() %>%
    mutate(
      prediction = rnorm(1, mu, sigma)                    # compute predictive distribution in backtransformed units
    )
}