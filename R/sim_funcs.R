
gen_sim_data <- function(n_groups = 100,
                         n_bills = 300,
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
           beta_j = case_when(rep == 1 ~ rnorm(n(), 0, beta_sd),
                              rep == 0 ~ rnorm(n(), 0, beta_sd)),
           gamma_j = case_when(rep == 1 ~ rnorm(n(), rep_effect, gamma_sd),
                               rep == 0 ~ rnorm(n(), 0, gamma_sd)))
  
  ij_all <- bills %>% 
    crossing(group_id = as.character(1:n_groups)) %>% 
    left_join(groups, by = "group_id") %>% 
    mutate(pr_y_ij_3 = pnorm(gamma_j * theta_i + beta_j - tau_i / sigma_j, sd = sigma_j^2),
           pr_y_ij_2 = pnorm(tau_i / sigma_j - (gamma_j * theta_i + beta_j), sd = sigma_j^2) -
             pnorm(-tau_i / sigma_j - (gamma_j * theta_i + beta_j), sd = sigma_j^2),
           pr_y_ij_1 = 1 - pnorm(gamma_j * theta_i + beta_j + tau_i / sigma_j, sd = sigma_j^2),
           y_ij_det = case_when(pr_y_ij_3 > pr_y_ij_2 & pr_y_ij_3 > pr_y_ij_1 ~ 3,
                                pr_y_ij_2 > pr_y_ij_3 & pr_y_ij_2 > pr_y_ij_1 ~ 2,
                                pr_y_ij_1 > pr_y_ij_3 & pr_y_ij_1 > pr_y_ij_2 ~ 1)) %>% 
    rowwise() %>% 
    mutate(y_ij = sample(c(1, 2, 3), 1, prob = c(pr_y_ij_1, pr_y_ij_2, pr_y_ij_3))) %>% 
    group_by(group_id) %>% 
    mutate(total_2s = sum(y_ij == 2)) %>% 
    ungroup() %>% 
    filter(total_2s != n_bills) 
  
  thetas <- ij_all %>% 
    select(group_id, theta_i) %>% 
    distinct()
  
  ij_obs <- ij_all %>% 
    filter(y_ij != 2)
  
  ij_obs_rc <- ij_obs %>% 
    mutate(yea = ifelse(y_ij == 3, 1, 0)) %>% 
    select(group_id, bill_id, yea) %>% 
    pivot_wider(id_cols = group_id,
                values_from = yea,
                names_from = bill_id) %>% 
    arrange(as.numeric(group_id)) %>% 
    select(-group_id) %>% 
    select(str_sort(names(.), numeric = TRUE)) %>% 
    rollcall()
  
  return(lst(ij_all, ij_obs, ij_obs_rc, thetas))
}
# a <- gen_sim_data()
# janitor::tabyl(sim_data[[5]]$ij_all$y_ij)
# a$ij_all %>%
#   mutate(voted = ifelse(y_ij %in% c(1, 3), 1, 0)) %>%
#   group_by(group_id) %>%
#   summarise(vote_avg = mean(voted), tau_i = mean(tau_i),
#             theta_i = mean(theta_i), business = mean(business)) %>%
#   ggplot(aes(x = tau_i, y = vote_avg)) +
#   geom_point()


