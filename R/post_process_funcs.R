
calc_cfl_qis <- function(ord_model, pscl_model, group_info_df) {
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
           method = "Traditional Model")
  
  all_qis <- rbind(
    ord_qis %>% select(group_id, theta_est, .lower, .upper, .width, 
                       method, plot_seq_ord, plot_seq_pscl),
    pscl_qis %>% select(group_id, theta_est, .lower, .upper, .width, 
                        method, plot_seq_ord, plot_seq_pscl)
  ) |> 
    left_join(group_info_df, by = "group_id") |> 
    mutate(all = "all")
  
  return(all_qis)
}

calc_group_posteriors <- function(ord_model, pscl_model, group_info_df) {
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
           method = "Traditional Model")
  
  all_draws <- rbind(
    ord_draws %>% select(group_id, theta_est, method),
    pscl_draws %>% select(group_id, theta_est, method)
  ) |> 
    left_join(group_info_df, by = "group_id")
  
  return(all_draws)
}
