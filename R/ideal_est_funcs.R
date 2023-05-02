



set_irt_formula_priors <- function(model_type) {
  if (model_type == "binary") {
    irt_formula <- bf(y_ij ~ gamma * theta + beta,
                      theta ~ 1 + business + (1 | group_id),
                      gamma ~ 1 + rep + (1 | bill_id),
                      beta ~ 1 + (1 | bill_id),
                      nl = TRUE)
    irt_family <- brmsfamily("bernoulli", "probit")
  } 
  if (model_type == "ordinal") {
    irt_formula <- bf(y_ij | thres(gr = group_id) ~ gamma * theta + beta,
                      theta ~ 1 + business + (1 | group_id),
                      gamma ~ 1 + rep + (1 | bill_id),
                      beta ~ 1 + (1 | bill_id),
                      nl = TRUE)
    irt_family <- brmsfamily("cumulative", "probit")
  }
  
  irt_priors_low_tau <-
    prior(constant(1), class = sd, nlpar = theta) +
    prior(cauchy(0, 2), class = sd, nlpar = gamma) +
    prior(cauchy(0, 2), class = sd, nlpar = beta) +
    prior(constant(.5), class = b, coef = business, nlpar = theta) +
    prior(constant(.5), class = b, coef = rep, nlpar = gamma) +
    prior(normal(0, 1), class = b, nlpar = gamma) +
    prior(normal(0, 1), class = b, nlpar = beta) +
    prior(normal(0, 1), class = b, nlpar = theta) +
    prior(normal(0, 2), class = Intercept)
  
  irt_priors_high_tau <-
    prior(constant(1), class = sd, nlpar = theta) +
    prior(cauchy(0, 2), class = sd, nlpar = gamma) +
    prior(cauchy(0, 2), class = sd, nlpar = beta) +
    prior(constant(.5), class = b, coef = business, nlpar = theta) +
    prior(constant(.5), class = b, coef = rep, nlpar = gamma) +
    prior(normal(0, 3), class = b, nlpar = gamma) +
    prior(normal(0, 2), class = b, nlpar = beta) +
    prior(normal(0, 3), class = b, nlpar = theta) +
    prior(normal(0, 4), class = Intercept)
  
  return(lst(irt_formula, irt_family, irt_priors_high_tau, irt_priors_low_tau))
}

set_inits <- function(seed) {
  set.seed(seed)
  list(b_gamma = runif(1, .1, 1),
       b_theta = runif(1, .1, 1))
}

run_brm_irt <- function(input_df, 
                        irt_formula, 
                        irt_priors,
                        model_type,
                        irt_family,
                        prior_predictive = "no") {
  
  if (model_type == "binary") {
    input_df <- input_df %>% 
      mutate(y_ij = ifelse(y_ij == 3, 1, 0))
  }
  
  fit <- brm(
    formula = irt_formula,
    family = irt_family,
    prior = irt_priors,
    data = input_df,
    seed = 222,
    iter = 2000,
    chains = 4, 
    cores = 4,
    # inits = list(
    #   set_inits(1),
    #   set_inits(2),
    #   set_inits(3),
    #   set_inits(4)
    # ),
    init = "0",
    sample_prior = prior_predictive,
    # adapt_delta = 0.95,
    # max_treedepth = 15,
    backend = "cmdstanr"
  )
  return(fit)
}

set_hurdle_irt_specs <- function() {
  
  irt_family <- custom_family(
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
  irt_stanvars <- stanvar(scode = stan_funs, block = "functions")
  
  irt_formula <- bf(position ~ gamma * theta + beta,
                  theta ~ busi + (1 | group_id),
                  gamma ~ rep + (1 | bill_id),
                  beta ~ (1 | bill_id),
                  hu ~ type_distance + rep + busi,
                  nl = TRUE)

  irt_priors <-
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
  
  return(lst(irt_family, irt_stanvars, irt_formula, irt_priors))
}

posterior_predict_hurdle_probit <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  theta <- brms::get_dpar(prep, "hu", i = i)
  
  hu <- runif(prep$ndraws, 0, 1)
  ifelse(hu < theta, 2, rbinom(prep$ndraws, 1, pnorm(mu)))
}

run_hurdle_irt <- function(input_df,
                           irt_formula,
                           irt_priors,
                           irt_family,
                           irt_stanvars) {
  fit <- brm(
    irt_formula,
    data = input_df,
    prior = irt_priors,
    family = irt_family,
    stanvars = irt_stanvars,
    backend = "cmdstanr",
    # init = "0",
    seed = 22,
    iter = 2000,
    chains = 4,
    threads = threading(2),
    cores = parallel::detectCores() - 2
  )
  return(fit)
}










