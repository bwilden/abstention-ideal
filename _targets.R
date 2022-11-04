library(targets)
library(tarchetypes)
library(here)

# Loading functions
source(here("R", "sim_funcs.R"))
source(here("R", "prep_funcs.R"))
source(here("R", "ideal_est_funcs.R"))

options(tidyverse.quiet = TRUE)
set.seed(111)
tar_option_set(packages = c("here",
                            "tidyverse",
                            "MetBrewer",
                            "lubridate",
                            "brms",
                            "tidybayes",
                            "ggdist",
                            "bayesplot",
                            "pscl",
                            "ggstance"))
suppressWarnings(library(tidyverse))

list(
  tar_target(
    clf_data,
    prep_clf_data()
  ),
  tar_map(
    values = tibble(tau_mean = c(0, 1, 2, 3, 4),
                    tau_rate = c(.5, .5, .5, .5, .5)),
    names = "tau_mean",
    tar_target(sim_data,
               gen_sim_data(tau_mean, tau_rate))
  ),
  tar_target(
    ord_irt_specs,
    set_irt_formula_priors("ordinal")
  )
)
