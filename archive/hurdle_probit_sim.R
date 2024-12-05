
library(dplyr)
library(brms)

dat <- tibble(
  x1 = rnorm(1000),
  x2 = rnorm(1000)
) |> 
  mutate(pr_missing = pnorm(x2),
         missing = rbinom(n(), 1, prob = pr_missing),
         pr_yes = pnorm(x1),
         yes = rbinom(n(), 1, prob = pr_yes),
         y = case_when(missing == 1 ~ 2,
                       yes == 1 ~ 1,
                       yes == 0 ~ 0))


hurdle_probit <- custom_family(
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

fit <- brm(
  bf(y ~ x1,
     hu ~ x2),
  prior = prior(normal(0, 2), class = b, coef = x1) +
    prior(normal(0, 2), class = b, coef = x2, dpar = hu),
  data = dat,
  family = hurdle_probit,
  stanvars = stanvars,
  backend = "cmdstanr",
  chains = 4,
  cores = 4
)

summary(fit)

glm(missing ~ x2, data = dat, family = binomial(link = "probit"))
glm(yes ~ x1, data = dat |> filter(missing == 0), family = binomial(link = "probit"))
