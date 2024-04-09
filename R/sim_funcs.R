
gen_sim_data_ord <- function(n_groups,
                             n_bills,
                             theta_sd = 1,
                             tau_rate = 0,
                             tau_mean = 0,
                             beta_sd =  1,
                             gamma_sd = 1,
                             sigma_j = 1,
                             business_effect = .5,
                             rep_effect = .5) {
  groups <- tibble(
    group_id = as.character(1:n_groups)
  ) %>% 
    mutate(business = ifelse(row_number() %% 3 == 0, 1, 0),
           theta_i = case_when(business == 1 ~ rnorm(n(), business_effect, theta_sd),
                               business == 0 ~ rnorm(n(), 0, theta_sd)),
           tau_i = abs(rnorm(n(), tau_mean, tau_rate)))
  
  bills <- tibble(
    bill_id = as.character(1:n_bills)
  ) %>% 
    mutate(rep = ifelse(row_number() %% 2 == 0, 1, 0),
           # beta should not be affected by partisanship
           beta_j = case_when(rep == 1 ~ rnorm(n(), 0, beta_sd),
                              rep == 0 ~ rnorm(n(), 0, beta_sd)),
           # but gamma is
           gamma_j = case_when(rep == 1 ~ rnorm(n(), rep_effect, gamma_sd),
                               rep == 0 ~ rnorm(n(), 0, gamma_sd)))
  
  ij_all <- bills %>% 
    crossing(group_id = as.character(1:n_groups)) %>% 
    left_join(groups, by = "group_id") %>% 
    mutate(pr_position_3 = pnorm(gamma_j * theta_i + beta_j - tau_i / sigma_j, sd = sigma_j^2),
           pr_position_2 = pnorm(tau_i / sigma_j - (gamma_j * theta_i + beta_j), sd = sigma_j^2) -
             pnorm(-tau_i / sigma_j - (gamma_j * theta_i + beta_j), sd = sigma_j^2),
           pr_position_1 = 1 - pnorm(gamma_j * theta_i + beta_j + tau_i / sigma_j, sd = sigma_j^2),
           position_det = case_when(pr_position_3 > pr_position_2 & pr_position_3 > pr_position_1 ~ 3,
                                pr_position_2 > pr_position_3 & pr_position_2 > pr_position_1 ~ 2,
                                pr_position_1 > pr_position_3 & pr_position_1 > pr_position_2 ~ 1)) %>% 
    rowwise() %>% 
    mutate(position = sample(c(1, 2, 3), 1, prob = c(pr_position_1, pr_position_2, pr_position_3))) %>% 
    group_by(group_id) %>% 
    mutate(total_2s = sum(position == 2)) %>% 
    ungroup() %>% 
    filter(total_2s != n_bills) 
  
  thetas <- pull_thetas(ij_all)
  
  ij_obs_rc <- ij_all |> 
    mutate(position = ifelse(position == 3, 1, 0)) |> 
    sim_to_rc()
  
  return(lst(ij_all, thetas, ij_obs_rc))
}


gen_sim_data_hurdle <- function(n_groups,
                                n_bills,
                                k_types,
                                rep_effect = .5) {
  groups <- tibble(
    group_id = as.character(1:n_groups),
    group_type = sample(1:k_types, n_groups, replace = TRUE)
  ) |> 
    mutate(theta_i = rnorm(n(), mean = group_type),
           theta_i = scale(theta_i),
           busi = ifelse(group_type > mean(group_type), 1, 0))
  
  bills <- tibble(
    bill_id = as.character(1:n_bills),
    bill_type = sample(1:k_types, n_bills, replace = TRUE)
  ) %>% 
    mutate(rep = ifelse(row_number() %% 2 == 0, 1, 0),
           beta_j = rnorm(n()),
           gamma_j = case_when(rep == 1 ~ rnorm(n(), rep_effect, sd = 1),
                               rep == 0 ~ rnorm(n(), -rep_effect, sd = 1)),
           gamma_j = rnorm(n(), mean = bill_type),
           gamma_j = scale(gamma_j),
           rep = ifelse(bill_type > mean(bill_type), 1, 0))
  
  ij_all <- bills |> 
    crossing(group_id = as.character(1:n_groups)) |>  
    left_join(groups, by = "group_id") |> 
    mutate(type_distance = ifelse(group_type == bill_type, 1, 0),
           pr_abstain = case_when(type_distance == 1 ~ rbeta(n(), 1, 9),
                                  type_distance == 0 ~ rbeta(n(), 9, 1)),
           abstain = rbinom(n(), 1, pr_abstain),
           pr_support = pnorm(gamma_j * theta_i + beta_j),
           support = rbinom(n(), 1, pr_support),
           position = case_when(abstain == 1 ~ 2,
                                support == 1 ~ 1,
                                support == 0 ~ 0))
  
  thetas <- pull_thetas(ij_all)
  
  ij_obs_rc <- sim_to_rc(ij_all)
  
  return(lst(ij_all, thetas, ij_obs_rc))
}

pull_thetas <- function(sim_all) {
  thetas <- sim_all %>% 
    select(group_id, theta_i) %>% 
    distinct()
  
  return(thetas)
}


sim_to_rc <- function(sim_all) {
  ij_obs <- sim_all %>% 
    filter(position != 2)
  
  ij_obs_rc <- ij_obs %>% 
    rename(yea = position) %>% 
    select(group_id, bill_id, yea) %>% 
    pivot_wider(id_cols = group_id,
                values_from = yea,
                names_from = bill_id) %>% 
    arrange(as.numeric(group_id)) %>% 
    select(-group_id) %>% 
    select(stringr::str_sort(names(.), numeric = TRUE)) %>% 
    pscl::rollcall()
  return(ij_obs_rc)
}


# set_hurdle_irt_specs <- function() {
#   
#   irt_family <- custom_family(
#     "hurdle_probit",
#     dpars = c("mu", "hu"),
#     links = c("identity", "probit"),
#     type = "int")
#   
#   stan_funs <- "
#   real hurdle_probit_lpmf(int y, real mu, real hu) {
#     if (y == 2) {
#       return bernoulli_lpmf(1 | hu);
#     } else {
#       return bernoulli_lpmf(0 | hu) +
#              bernoulli_lpmf(y | Phi(mu));
#     }
#   }
# "
#   irt_stanvars <- stanvar(scode = stan_funs, block = "functions")
#   
#   irt_formula <- bf(position ~ gamma * theta + beta,
#                     theta ~  (1 | group_id),
#                     gamma ~  rep + (1 | bill_id),
#                     beta ~ (1 | bill_id),
#                     hu ~ group_type,
#                     nl = TRUE)
#   
#   irt_priors <-
#     prior(cauchy(0, 5), class = sd, nlpar = theta) +
#     prior(constant(1), class = sd, nlpar = gamma) +
#     prior(cauchy(0, 5), class = sd, nlpar = beta) +
#     prior(normal(0, 2), class = b, nlpar = beta) +
#     prior(normal(0, 2), class = b, nlpar = gamma) +
#     prior(normal(0, 3), class = b, nlpar = theta) +
#     prior(normal(0, 1), class = b, dpar = hu) +
#     prior(normal(0, 1), class = Intercept, dpar = hu) +
#     prior(constant(.1), class = b, coef = rep, nlpar = gamma)
#   
#   return(lst(irt_family, irt_stanvars, irt_formula, irt_priors))
# }

# a<-gen_sim_data(n_groups = 75, n_bills = 150, k_types = 5)
# a$ij_all |> janitor::tabyl(position)
# a$ij_all |> group_by(group_type) |> summarise(mean(abstain))


# fit <- brm(
#   set_hurdle_irt_specs()$irt_formula,
#   data = a$ij_all,
#   prior = set_hurdle_irt_specs()$irt_priors,
#   family = set_hurdle_irt_specs()$irt_family,
#   stanvars = set_hurdle_irt_specs()$irt_stanvars,
#   backend = "cmdstanr",
#   init = "0",
#   iter = 2000,
#   chains = 4,
#   threads = threading(4),
#   cores = parallel::detectCores()
# )
# 
# summary(fit)
# 
# sim_checks(fit, a$ij_all)
# pp_check(fit)
# 
# 
# get_prior(set_hurdle_irt_specs()$irt_formula, family = set_hurdle_irt_specs()$irt_family, data = a$ij_all)


