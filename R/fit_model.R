library(tidyverse)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(gt)
library(gtExtras)

# read data
gallup_data <- read_csv(here("data", "gallup_data.csv"))
maskina_data <- read_csv(here("data", "maskina_data.csv"))
prosent_data <- read_csv(here("data", "prosent_data.csv"))
felagsvisindastofnun_data <- read_csv(here("data", "felagsvisindastofnun_data.csv"))
election_data <- read_csv(here("data", "election_data.csv"))
# combine data
data <- bind_rows(
  maskina_data,
  prosent_data,
  felagsvisindastofnun_data,
  gallup_data,
  election_data
) |>
  mutate(
    flokkur = if_else(flokkur == "Lýðræðisflokkurinn", "Annað", flokkur),
    fyrirtaeki = fct_relevel(
      as_factor(fyrirtaeki),
      "Kosning"
    )
  ) |>
  filter(
    date >= clock::date_build(2021, 1, 1),
    flokkur != "Annað"
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


fit$summary("y_rep", mean, ~ quantile(.x, probs = seq(0.05, 0.95, by = 0.05))) |>
  pivot_longer(c(-variable, -mean)) |>
  mutate(
    prob = parse_number(name),
    coverage = 2 * abs(50 - prob),
    which = if_else(prob < 50, "lower", "upper")
  ) |>
  filter(prob != 50) |>
  select(-prob, -name) |>
  pivot_wider(names_from = which, values_from = value) |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
    dags = unique(data$date)[t]
  ) |>
  group_by(dags, coverage) |>
  mutate_at(
    vars(mean, lower, upper),
    ~ .x / sum(.x)
  ) |>
  ungroup() |>
  left_join(
    data |>
      mutate(p = n / sum(n), .by = c(date, fyrirtaeki)) |>
      select(dags = date, fyrirtaeki, flokkur, konnun = p),
    by = join_by(dags, flokkur),
    relationship = "many-to-many"
  ) |>
  ggplot(aes(dags, mean)) +
  stat_smooth(
    geom = "line",
    method = "loess",
    span = 0.5,
    aes(col = flokkur),
    linewidth = 1
  ) +
  geom_point(aes(y = konnun, col = flokkur))



fit$draws("y_rep") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    t = str_match(name, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
    dags = unique(data$date)[t]
  ) |>
  filter(dags == max(dags)) |>
  mutate(
    value = value / sum(value),
    .by = c(.iteration, .chain, .draw)
  ) |>
  summarise(
    mean = mean(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = flokkur
  ) |>
  select(flokkur, mean, q5, q95) |>
  arrange(desc(mean)) |>
  mutate(
    plot_col = mean
  ) |>
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
  cols_align(
    align = "left",
    columns = 1
  ) |>
  fmt_percent() |>
  gt_color_rows(
    -1,
    palette = "Greys",
    domain = c(0, 0.5)
  ) |>
  cols_nanoplot(
    columns = plot_col,
    plot_type = "bar",
    after = 1,
    autoscale = TRUE,
    new_col_label = "Fylgi",
    options = nanoplot_options(
      data_bar_fill_color = "grey",
      data_bar_stroke_color = "grey",
      data_bar_stroke_width = 0.5
    )
  ) |>
  tab_header(
    title = "Fylgi stjórnmálaflokka samkvæmt nýjustu könnunum"
  ) |>
  tab_footnote(
    "Niðurstöður kannanar eru vegnar saman með stigskipti Bayesísku líkani"
  )


y_rep_draws <- fit$draws("y_rep") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    t = str_match(name, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
    dags = unique(data$date)[t]
  ) |>
  select(
    .chain,
    .iteration,
    .draw,
    dags,
    flokkur,
    value
  )
write_csv(y_rep_draws, here("data", "y_rep_draws.csv"))


theme_set(metill::theme_metill())

fit$summary("gamma") |>
  select(variable, mean) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
    fyrirtaeki = unique(data$fyrirtaeki)[h]
  ) |>
  ggplot(aes(0, flokkur, col = fyrirtaeki)) +
  geom_segment(
    aes(xend = mean, yend = flokkur),
    position = position_jitter(width = 0, height = 0.2),
    arrow = arrow(length = unit(0.4, "cm"), type = "closed")
  )
