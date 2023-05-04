
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


make_sim_comparison_plot <- function(brm_model, pscl_model, true_thetas) {
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
  
  brm_draws <- brm_model %>% 
    spread_draws(r_group_id__theta[r_group_id, ]) %>% 
    mean_qi(.width = .89) %>% 
    mutate(group_id = as.character(r_group_id),
           theta_est = r_group_id__theta,
           method = "Abstension Model") %>% 
    left_join(true_thetas) %>% 
    select(-c(r_group_id, r_group_id__theta))
  
  theta_draws <- rbind(
    pscl_draws,
    brm_draws
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
