
set_irt_specs <- function(model_type) {
  if (model_type == "ordinal_probit") {
    formula <- bf(position | thres(gr = group_id) ~ gamma * theta + beta,
                  theta ~ 1 + business + (1 | group_id),
                  gamma ~ 1 + rep + (1 | bill_id),
                  beta ~ 1 + (1 | bill_id),
                  nl = TRUE)
    family <- brmsfamily("cumulative", "probit")
  
    priors_low_tau <-
      prior(constant(1), class = sd, nlpar = theta) +
      prior(cauchy(0, 2), class = sd, nlpar = gamma) +
      prior(cauchy(0, 2), class = sd, nlpar = beta) +
      prior(constant(.5), class = b, coef = business, nlpar = theta) +
      prior(constant(.5), class = b, coef = rep, nlpar = gamma) +
      prior(normal(0, 1), class = b, nlpar = gamma) +
      prior(normal(0, 1), class = b, nlpar = beta) +
      prior(normal(0, 1), class = b, nlpar = theta) +
      prior(normal(0, 2), class = Intercept)
    
    priors_high_tau <-
      prior(constant(1), class = sd, nlpar = theta) +
      prior(cauchy(0, 2), class = sd, nlpar = gamma) +
      prior(cauchy(0, 2), class = sd, nlpar = beta) +
      prior(constant(.5), class = b, coef = business, nlpar = theta) +
      prior(constant(.5), class = b, coef = rep, nlpar = gamma) +
      prior(normal(0, 3), class = b, nlpar = gamma) +
      prior(normal(0, 2), class = b, nlpar = beta) +
      prior(normal(0, 3), class = b, nlpar = theta) +
      prior(normal(0, 4), class = Intercept)
    
    priors <- list(priors_high_tau, priors_low_tau)
    stanvars <- NULL
  }
  
  if (model_type == "hurdle_probit") {
    family <- custom_family(
      "hurdle_probit",
      dpars = c("mu", "hu"),
      links = c("identity", "probit"),
      type = "int")
    
    stan_funs <- "
      real hurdle_probit_lpmf(int y, real mu, real hu) {
        if (y == 2) {
          return bernoulli_lpmf(1 | hu);
        } else {
          return bernoulli_lpmf(0 | hu) +
                 bernoulli_lpmf(y | Phi(mu));
        }
      }
    "
    stanvars <- stanvar(scode = stan_funs, block = "functions")
    
    formula <- bf(position ~ gamma * theta + beta,
                  theta ~ busi + (1 | group_id),
                  gamma ~ rep + (1 | bill_id),
                  beta ~ (1 | bill_id),
                  hu ~ type_distance + rep + busi,
                  nl = TRUE)
    
    priors <-
      prior(cauchy(0, 2), class = sd, nlpar = theta) +
      prior(cauchy(0, 2), class = sd, nlpar = gamma) +
      prior(cauchy(0, 1), class = sd, nlpar = beta) +
      prior(normal(0, 2), class = b, nlpar = beta) +
      prior(normal(0, 2), class = b, nlpar = gamma) +
      prior(normal(0, 3), class = b, nlpar = theta) +
      prior(normal(0, 1), class = b, dpar = hu) +
      prior(normal(0, 1), class = Intercept, dpar = hu) +
      prior(constant(.5), class = b, coef = rep, nlpar = gamma) +
      prior(constant(.5), class = b, coef = busi, nlpar = theta)
  }
  
  return(lst(family, stanvars, formula, priors))
}

posterior_predict_hurdle_probit <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  theta <- brms::get_dpar(prep, "hu", i = i)
  
  hu <- runif(prep$ndraws, 0, 1)
  ifelse(hu < theta, 2, rbinom(prep$ndraws, 1, pnorm(mu)))
}

run_brm_irt <- function(input_df,
                        irt_formula,
                        irt_priors,
                        irt_family,
                        irt_stanvars,
                        ...) {
  fit <- brm(
    irt_formula,
    data = input_df,
    prior = irt_priors,
    family = irt_family,
    stanvars = irt_stanvars,
    backend = "cmdstanr",
    seed = 22,
    iter = 2000,
    chains = 4,
    threads = threading(2),
    cores = parallel::detectCores() - 2,
    ...
  )
  return(fit)
}







