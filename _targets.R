
library(targets)

# Loading functions
tar_source("R")

tar_option_set(packages = c("here",
                            "tidyverse",
                            "MetBrewer",
                            "lubridate",
                            "brms",
                            "tidybayes",
                            "ggdist",
                            "bayesplot",
                            "pscl",
                            "ggstance",
                            "patchwork"))
set.seed(111)

list(
  # Simulations - Hurdle
  # tar_target(
  #   sim_data_hurdle,
  #   gen_sim_data_hurdle(n_groups = 50, 
  #                       n_bills = 100,
  #                       k_types = 10000)
  # ),
  # tar_target(
  #   hurdle_irt_specs,
  #   set_irt_specs("hurdle_probit")
  # ),
  # tar_target(
  #   hurdle_irt,
  #   run_brm_irt(sim_data_hurdle$ij_all,
  #               irt_priors = hurdle_irt_specs$priors,
  #               irt_stanvars = hurdle_irt_specs$stanvars,
  #               irt_formula = hurdle_irt_specs$formula,
  #               irt_family = hurdle_irt_specs$family)
  #   
  # ),
  # tar_target(
  #   hurdle_irt_checks,
  #   sim_checks(hurdle_irt, sim_data_hurdle$ij_all)
  # ),
  # tar_target(
  #   pscl_hurdle_irt,
  #   ideal(sim_data_hurdle$ij_obs_rc,
  #         maxiter = 12500,
  #         burnin = 7500,
  #         dropList = list(lop = NA),
  #         normalize = TRUE)
  # ),
  # tar_target(
  #   sim_hurdle_comparison_plot,
  #   make_sim_comparison_plot(hurdle_irt, 
  #                            pscl_hurdle_irt, 
  #                            sim_data_hurdle$thetas)
  # ),
  # Simulations - Ordinal
  tar_target(
    sim_data_ord,
    tibble(tau_mean = c(0, 1, 2, 3, 4),
           tau_rate = c(.5, .5, .5, .5, .5)) |> 
      pmap(gen_sim_data_ord,
           n_groups = 100,
           n_bills = 300)
  ),
  tar_target(
    ord_irt_specs,
    set_irt_specs("ordinal_probit")
  ),
  tar_target(
    ord_irt,
    pmap(tibble(input_df = map(sim_data_ord, ~.x$ij_all),
                irt_priors = list(ord_irt_specs$priors$low_tau,
                                  ord_irt_specs$priors$low_tau,
                                  ord_irt_specs$priors$high_tau,
                                  ord_irt_specs$priors$high_tau,
                                  ord_irt_specs$priors$high_tau)),
         .f = run_brm_irt,
         irt_formula = ord_irt_specs$formula,
         irt_family = ord_irt_specs$family)
  ),
  tar_target(
    pscl_ord_irt,
    map(map(sim_data_ord, ~.x$ij_obs_rc),
        ideal,
        maxiter = 12500,
        burnin = 7500,
        dropList = list(lop = NA),
        normalize = TRUE)
  ),
  tar_target(
    sim_ord_comparison_plots,
    tibble(brm_model = ord_irt,
           pscl_model = pscl_ord_irt,
           true_thetas = map(sim_data_ord, ~.x$thetas)) |> 
    pmap(.f = make_sim_comparison_plot)
  )
  
  # CFL replication
  # tar_target(
  #   cfl_group_info,
  #   get_cfl_group_info(cfl_posteriors_file)
  # ),
  # tar_target(
  #   cfl_data,
  #   prep_cfl_data(cfl_group_info)
  # ),
  # tar_target(
  #   cfl_exp_data,
  #   map(.x = c("113"),
  #       .f = expand_group_dispositions,
  #       groups_df = cfl_data$groups,
  #       # top_n = 15,,
  #       n_groups = 200,
  #       n_bills = 600)
  # ),
  # tar_target(
  #   cfl_posteriors_file,
  #   here::here("data-raw", 
  #              "Polarized_Pluralism_Replication", 
  #              "Data", 
  #              "group_fullchains_final.csv"),
  #   format = "file"
  # ),
  # tar_target(
  #   cfl_pscl_irt,
  #   map(map(cfl_exp_data, ~.x$ij_obs_rc),
  #       ideal,
  #       maxiter = 125000,
  #       burnin = 75000,
  #       dropList = list(lop = NA),
  #       normalize = TRUE)
  # ),
  # tar_target(
  #   cfl_ord_irt,
  #   map(map(cfl_exp_data, ~.x$ij_all),
  #       run_brm_irt,
  #       irt_formula = ord_irt_specs$formula,
  #       irt_family = ord_irt_specs$family,
  #       irt_priors = ord_irt_specs$priors$high_tau)
  # ),
  # # tar_target(
  # #   cfl_ord_irt_checks,
  # #   check_brms_model(cfl_ord_irt[[1]])
  # # ),
  # tar_target(
  #   cfl_qis,
  #   calc_cfl_qis(cfl_ord_irt[[1]],
  #                cfl_pscl_irt[[1]],
  #                group_info_df = cfl_group_info)
  # ),
  # tar_target(
  #   cfl_comparison_plot,
  #   map(c("ord", "pscl"),
  #       make_cfl_comparison_plot,
  #       qis = cfl_qis)
  # ),
  # # Fig3
  # tar_target(
  #   cfl_density_plot,
  #   make_cfl_density_plot(cfl_qis)
  # ),
  # tar_target(
  #   cfl_draws,
  #   calc_group_posteriors(cfl_ord_irt[[1]],
  #                         cfl_pscl_irt[[1]],
  #                         group_info_df = cfl_group_info)
  # ),
  # tar_target(
  #   cfl_disagree_groups,
  #   make_group_posteriors_plot(cfl_draws,
  #                              selected_groups = c("National Employment Law Project",
  #                                                  "Americans for Tax Reform",
  #                                                  "Brennan Center for Justice",
  #                                                  "Sierra Club",
  #                                                  "Associated Builders & Contractors",
  #                                                  "Heritage Action for America"))
  # ),
  # tar_target(
  #   cfl_agree_groups,
  #   make_group_posteriors_plot(cfl_draws,
  #                              selected_groups = c("American Civil Liberties Union",
  #                                                  "United Steelworkers",
  #                                                  "US Chamber of Commerce",
  #                                                  "Center for American Progress",
  #                                                  "United Automobile Workers",
  #                                                  "AFL-CIO"))
  # ),
  # # Fig2
  # tar_target(
  #   chamber_compare_plot,
  #   make_group_posteriors_plot(cfl_draws,
  #                              selected_groups = c("US Chamber of Commerce",
  #                                                  "National Black Chamber of Commerce",
  #                                                  "California Chamber of Commerce",
  #                                                  "US Hispanic Chamber of Commerce",
  #                                                  "National Gay & Lesbian Chamber of Commerce",
  #                                                  "US Womens Chamber of Commerce"))
  # ),
  # # Fig4
  # tar_target(
  #   cfl_group_type_plot,
  #   make_group_qis_plot(cfl_qis,
  #                       type_category = "usecode",
  #                       selected_types = c("Corporations", "Health", 
  #                         "Education", "Occupational Associations",
  #                         "Trade and Other Business Associations", 
  #                         "Socail Welfare or Poor", "Public Interest", 
  #                         "Identity Groups", "Unions")
  #   )
  # ),
  # tar_target(
  #   cfl_sector_plot,
  #   make_group_qis_plot(cfl_qis,
  #                       type_category = "Sector",
  #                       selected_types = cfl_qis |> 
  #                         filter(!(Sector %in% c("Ideology/Single-Issue", 
  #                                                "Party Cmte", "Unknown", 
  #                                                "Other", "Non-contribution")),
  #                                !is.na(Sector)) |> 
  #                         pull(Sector))
  # ),
  # tar_target(
  #   cfl_single_issue_plot,
  #   make_group_qis_plot(cfl_qis |> 
  #                         filter(Sector == "Ideology/Single-Issue"),
  #                       type_category = "Catname",
  #                       selected_types = cfl_qis |>
  #                         filter(Sector == "Ideology/Single-Issue") |> 
  #                         pull(Catname))
  # )
)
