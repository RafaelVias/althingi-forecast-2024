library(tidyverse)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(gt)
library(gtExtras)

# read data
sheets_data <- read_csv(here("data", "sheets_data.csv"))
gallup_data <- read_csv(here("data", "gallup_data.csv"))

# combine data
data <- bind_rows(
  sheets_data,
  gallup_data |>
    filter(
      date >= clock::date_build(2024, 9, 1)
    ) |>
    mutate(
      date = clock::date_build(2024, 10, 1)
    )
) |>
  mutate(
    flokkur = if_else(flokkur == "Lýðræðisflokkurinn", "Annað", flokkur)
  ) |>
  filter(
    date == clock::date_build(2024, 10, 1)
  ) |>
  arrange(date, fyrirtaeki, flokkur)

T <- length(unique(data$date))
P <- length(unique(data$flokkur))
H <- length(unique(data$fyrirtaeki))
N <- data |>
  distinct(fyrirtaeki, date) |>
  nrow()

y <- data |>
  select(date, fyrirtaeki, flokkur, n) |>
  mutate(
    n = as.integer(n)
  ) |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  select(-date, -fyrirtaeki) |>
  as.matrix()

house <- data |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  mutate(
    house = as.numeric(factor(fyrirtaeki))
  ) |>
  pull(house)

date <- data |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  mutate(
    date = as.numeric(factor(date))
  ) |>
  pull(date)

stan_data <- list(
  T = T,
  P = P,
  H = H,
  N = N,
  y = y,
  house = house,
  date = date
)

model <- cmdstan_model(
  here("Stan", "base_model.stan")
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  max_treedepth = 15
)


fit$summary("gamma")

mcmc_areas(
  as_draws_matrix(fit$draws("gamma")),
  regex_pars = "gamma",
)

mcmc_areas(
  as_draws_matrix(fit$draws("beta")),
  regex_pars = "beta",
)

mcmc_areas(
  as_draws_matrix(fit$draws("y_now")),
  regex_pars = "y_now",
)

fit$draws("pi_now") |>
  as_draws_df() |>
  summarise_draws() |>
  mutate(
    flokkur = colnames(y),
    flokkur = fct_reorder(flokkur, mean)
  ) |>
  select(flokkur, mean, q5, q95) |>
  gt() |>
  cols_label(
    flokkur = "Flokkur",
    mean = "Væntigildi",
    q5 = "Neðri",
    q95 = "Efri"
  ) |>
  tab_spanner(
    label = "95% Öryggisbil",
    columns = c(q5, q95)
  ) |>
  fmt_percent() |>
  gt_color_rows(
    -1,
    palette = "Greys"
  )
