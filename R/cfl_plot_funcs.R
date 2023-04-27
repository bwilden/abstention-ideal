
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
