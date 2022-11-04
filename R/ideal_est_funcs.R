

calc_sim_predictions <- function(fit_ordinal, fit_binary, group_df) {
  group_coef_ordinal <- coef(fit_ordinal)$group_id[, , "theta_Intercept"] %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "group_id") %>% 
    rename_with(~ paste0(., "_ordinal"), -group_id)
  group_coef_binary <- coef(fit_binary)$group_id[, , "theta_Intercept"] %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "group_id") %>% 
    rename_with(~ paste0(., "_binary"), -group_id)
  
  group_coefs <- group_coef_binary %>% 
    left_join(group_coef_ordinal) %>% 
    left_join(group_df) %>% 
    mutate(in95_binary = ifelse(theta > Q2.5_binary & theta < Q97.5_binary, 1, 0),
           in95_ordinal = ifelse(theta > Q2.5_ordinal & theta < Q97.5_ordinal, 1, 0),
           pred_diff_binary = theta - Estimate_binary,
           pred_diff_ordinal = theta - Estimate_ordinal)
  
  in95_binary_mean <- mean(group_coefs$in95_binary)
  in95_ordinal_mean <- mean(group_coefs$in95_ordinal)
  pred_diff_binary_mean <- mean(group_coefs$pred_diff_binary)
  pred_diff_ordinal_mean <- mean(group_coefs$pred_diff_ordinal)
  
  return(lst(in95_binary_mean,
             in95_ordinal_mean,
             pred_diff_binary_mean,
             pred_diff_ordinal_mean))
  
}

sim_checks <- function(fitted_model, df) {
  coef_fit <- coef(fitted_model, probs = c(0.025, 0.25, 0.75, 0.975))
  
  p1 <- coef_fit$group_id[, , "theta_Intercept"] %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    select(-Est.Error) %>%
    arrange(Estimate) %>%
    mutate(group = seq_len(n())) %>%
    ggplot(aes(group, Estimate)) +
    geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.25) +
    geom_pointrange(aes(ymin = Q25, ymax = Q75), alpha = .5) +
    coord_flip() +
    theme_minimal() +
    labs(x = "Person Number (Sorted)")
  
  p2 <- coef_fit$group_id[, , "theta_Intercept"] %>%
    as.data.frame() %>%
    rownames_to_column() %>% 
    left_join(df %>% 
                select(group_id, theta = theta_i) %>% distinct(), by = c("rowname" = "group_id")) %>% 
    ggplot(aes(x = theta, y = Estimate)) +
    geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.25, col = "cadetblue") +
    geom_pointrange(aes(ymin = Q25, ymax = Q75), col = "cadetblue", alpha = .5) +
    geom_smooth(method = "lm", se = F, col = "black", linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()
  
  p3 <- coef_fit$bill_id[, , "beta_Intercept"] %>%
    as.data.frame() %>%
    rownames_to_column() %>% 
    left_join(df %>% select(bill_id, beta = beta_j) %>% distinct(), by = c("rowname" = "bill_id")) %>% 
    ggplot(aes(x = beta, y = Estimate)) +
    geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.25, col = "cadetblue") +
    geom_pointrange(aes(ymin = Q25, ymax = Q75), col = "cadetblue", alpha = .5) +
    geom_smooth(method = "lm", se = F, linetype = "dashed", col = "black") +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()
  
  p4 <- coef_fit$bill_id[, , "gamma_Intercept"] %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    left_join(df %>% select(bill_id, gamma = gamma_j) %>% distinct(), by = c("rowname" = "bill_id")) %>%
    ggplot(aes(x = gamma, y = Estimate)) +
    geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.25, col = "cadetblue") +
    geom_pointrange(aes(ymin = Q25, ymax = Q75), col = "cadetblue", alpha = .5) +
    geom_smooth(method = "lm", se = F, col = "black", linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()
  
  p <- (p1 + p2) / (p3 + p4)
  return(p)
}

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


# tar_load(ord_irt)
# tar_load(sim_data)
# sim_checks(ord_irt[[1]], sim_data[[1]]$ij_all)
# sim_checks(ord_irt[[2]], sim_data[[2]]$ij_all)
# sim_checks(ord_irt[[3]], sim_data[[3]]$ij_all)
# sim_checks(ord_irt[[4]], sim_data[[4]]$ij_all)
# sim_checks(ord_irt[[5]], sim_data[[5]]$ij_all)


# fit_bin <- run_sim_irt(a$ij_obs,
#                        formula_binary,
#                        prior_binary,
#                        inits_list = inits_list,
#                        "binary")
# fit_ord <- run_sim_irt(a$ij_all, 
#                        formula_ordinal,
#                        prior_ordinal,
#                        inits_list = inits_list,
#                        "ordinal",
#                        prior_predictive = "no")
# 
# summary(fit_ord)
# plot(fit_ord, ask = F, variable = c("b_gamma_Intercept",
#                                     "b_theta_Intercept",
#                                     "b_beta_Intercept",
#                                     "b_theta_business",
#                                     "sd_bill_id__gamma_Intercept"))
# mcmc_rhat_hist(rhat(fit_ord))
# mcmc_neff_hist(neff_ratio(fit_ord))
# pp_check(fit_ord)
# 
# sim_checks(fit_bin, a$ij_obs)
# sim_checks(fit_ord, a$ij_all)
# 
# 
# calc_sim_predictions(fit_ord, fit_bin, a$ij_all %>% select(group_id, theta = theta_i) %>% distinct())

sim_checks_pscl <- function(ideal_obj, theta_df) {
  theta_draws <- as_tibble(ideal_obj$x) %>% 
    pivot_longer(cols = everything(),
                 names_to = "group_id",
                 values_to = "Estimate") %>% 
    group_by(group_id) %>% 
    mean_qi() %>% 
    mutate(group_id = str_remove_all(group_id, "Legislator "),
           group_id = str_remove_all(group_id, ".D1")) %>% 
    ungroup() %>% 
    left_join(theta_df)
  
  p1 <- theta_draws %>% 
    arrange(Estimate) %>%
    mutate(group = seq_len(n())) %>%
    ggplot(aes(group, Estimate)) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper), alpha = .5) +
    coord_flip() +
    theme_minimal() +
    labs(x = "Group Number (Sorted)")
  
  p2 <- theta_draws %>% 
    ggplot(aes(x = theta_i, y = Estimate)) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper), col = "cadetblue", alpha = .5) +
    geom_smooth(method = "lm", se = F, col = "black", linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()
  
  p <- p1 + p2
  return(p)
}
# cl <- a$ij_obs_rc %>%
#   constrain.legis(
#     x = list("Legislator 91" = -3.24,
#              "Legislator 33" = 2.17))
# 
# sim_checks_pscl(ideal(
#   a$ij_obs_rc,
#   thin = 10,
#   priors = cl,
#   startvals = cl,
#   normalize = TRUE
# ),
# a$thetas)
# 
# a$thetas %>% filter(theta_i %in% c(max(theta_i), min(theta_i)))



# 
# tar_load(pscl_irt)
# sim_checks_pscl(pscl_irt[[1]], sim_data[[1]]$thetas)
# sim_checks_pscl(pscl_irt[[2]], sim_data[[2]]$thetas)
# sim_checks_pscl(pscl_irt[[3]], sim_data[[3]]$thetas)
# sim_checks_pscl(pscl_irt[[4]], sim_data[[4]]$thetas)
# sim_checks_pscl(pscl_irt[[5]], sim_data[[5]]$thetas)


make_sim_comparison_plot <- function(ord_model, pscl_model, true_thetas) {
  pscl_draws <- as_tibble(pscl_model$x) %>% 
    pivot_longer(cols = everything(),
                 names_to = "group_id",
                 values_to = "theta_est") %>% 
    group_by(group_id) %>% 
    mean_qi(.width = .89) %>% 
    mutate(group_id = str_remove_all(group_id, "Legislator "),
           group_id = str_remove_all(group_id, ".D1")) %>% 
    ungroup() %>% 
    left_join(true_thetas) %>% 
    na.omit() %>% 
    mutate(method = "pscl Model")
  
  if (cor(pscl_draws$theta_est, pscl_draws$theta_i) < 0) {
    pscl_draws <- pscl_draws %>%
      mutate(theta_est = -theta_est,
             .lower = -.lower,
             .upper = -.upper)
  }
  
  ord_draws <- ord_model %>% 
    spread_draws(r_group_id__theta[r_group_id, ]) %>% 
    mean_qi(.width = .89) %>% 
    mutate(group_id = as.character(r_group_id),
           theta_est = r_group_id__theta,
           method = "Abstension Model") %>% 
    left_join(true_thetas) %>% 
    select(-c(r_group_id, r_group_id__theta))
  
  theta_draws <- rbind(
    pscl_draws,
    ord_draws
  )
  
  p <- theta_draws %>%
    ggplot(aes(x = theta_i, y = theta_est, color = method)) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1) +
    theme_ggdist() +
    scale_color_manual(values = rev(met.brewer("Isfahan1", n = 2))) +
    labs(x = latex2exp::TeX("$\\theta_i$"), 
         y = latex2exp::TeX("$\\hat{\\theta}_i$"),
         caption = "Posterior medians and 89% intervals") +
    facet_wrap(~ method) +
    theme(axis.title.y = element_text(angle = 0, vjust = .5, hjust = .5),
          legend.position = "none",
          text = element_text(family = "serif")) 
  
  return(p)
}

check_brms_model <- function(brm_mod) {
  rhats <- mcmc_rhat_hist(rhat(brm_mod)) +
    theme_ggdist()
  neffs <- mcmc_neff_hist(neff_ratio(brm_mod)) +
    theme_ggdist()
  
  return(lst(rhats, neffs))
}

calc_cfl_qis <- function(ord_model, pscl_model) {
  ord_qis <- ord_model  %>%
    spread_draws(r_group_id__theta[r_group_id, ]) %>%
    median_qi(.width = .89) %>%
    arrange(r_group_id__theta) %>% 
    mutate(plot_seq_pscl = NA,
           plot_seq_ord = seq_len(n()),
           group_id = str_replace_all(r_group_id, "\\.", " "),
           theta_est = r_group_id__theta,
           method = "Abstension Model")
  
  pscl_qis <- as_tibble(pscl_model$x) %>% 
    pivot_longer(cols = everything(),
                 names_to = "group_id",
                 values_to = "theta_est") %>% 
    group_by(group_id) %>% 
    median_qi(.width = .89) %>% 
    arrange(theta_est) %>% 
    mutate(plot_seq_pscl = seq_len(n()),
           plot_seq_ord = NA,
           group_id = str_remove(group_id, ".D1"),
           method = "pscl Model")
  
  all_qis <- rbind(
    ord_qis %>% select(group_id, theta_est, .lower, .upper, .width, 
                       method, plot_seq_ord, plot_seq_pscl),
    pscl_qis %>% select(group_id, theta_est, .lower, .upper, .width, 
                        method, plot_seq_ord, plot_seq_pscl)
  )
  
  return(all_qis)
}

make_cfl_comparison_plot <- function(qis, ref_model) {
  if (ref_model == "ord") {
    qis$plot_seq <- qis$plot_seq_ord
  }
  if (ref_model == "pscl") {
    qis$plot_seq <- qis$plot_seq_pscl
  }
  
  p <- qis  %>% 
    group_by(group_id) %>% 
    mutate(plot_seq = max(plot_seq, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(plot_seq != -Inf) %>% 
    ggplot(aes(x = theta_est, y = plot_seq, color = method)) +
    geom_point() +
    geom_pointrangeh(aes(xmin = .lower, xmax = .upper), fatten = 0, alpha = .25) +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_color_manual(values = rev(met.brewer("Isfahan1", n = 2))) +
    theme_ggdist() +
    labs(x = expression(theta),
         y = expression("Groups Ordered by Abstention Model " ~ theta),
         caption = "Posterior medians and 89% intervals") +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          text = element_text(family = "serif"))
  
  return(p)
}

make_cfl_density_plot <- function(qis) {
  p <- qis %>% 
    ggplot(aes(x = theta_est, fill = method)) +
    geom_density(alpha = .75) +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", n = 2))) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_ggdist() +
    labs(x = expression("Median " ~ theta)) +
    xlim(-4, 3) +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          text = element_text(family = "serif"),
          axis.line.y.left = element_blank())
  
  return(p)
}

calc_group_posteriors <- function(ord_model, pscl_model) {
  ord_draws <- ord_model  %>%
    spread_draws(r_group_id__theta[r_group_id, ]) %>% 
    mutate(group_id = str_replace_all(r_group_id, "\\.", " "),
           theta_est = r_group_id__theta,
           method = "Abstension Model") %>% 
    ungroup()
  
  pscl_draws <- as_tibble(pscl_model$x) %>% 
    pivot_longer(cols = everything(),
                 names_to = "group_id",
                 values_to = "theta_est") %>% 
    mutate(group_id = str_remove(group_id, ".D1"),
           method = "pscl Model")
  
  all_draws <- rbind(
    ord_draws %>% select(group_id, theta_est, method),
    pscl_draws %>% select(group_id, theta_est, method)
  )
  
  return(all_draws)
}

make_group_posteriors_plot <- function(draws, selected_groups) {
  p <- draws %>% 
    filter(group_id %in% selected_groups) %>% 
    ggplot(aes(y = forcats::fct_reorder(group_id,
                                        theta_est,
                                        .fun = mean,
                                        .desc = TRUE),
               x = theta_est, fill = method)) +
    stat_dist_halfeye(alpha = .75, .width = .89, size = 1) +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", n = 2))) +
    theme_ggdist() +
    labs(x = expression(theta),
         y = "") +
    xlim(-4, 3) +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          text = element_text(family = "serif"))
  
  return(p)
}
