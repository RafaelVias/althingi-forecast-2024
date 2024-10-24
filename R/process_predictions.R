library(tidyverse)
library(googlesheets4)

dhondt <- function(votes, n_seats = 63) {
  n_seats <- unique(n_seats)
  party_seats <- numeric(length(votes))
  temp_votes <- votes

  while (sum(party_seats) < n_seats) {
    which_max <- which.max(temp_votes)
    party_seats[which_max] <- party_seats[which_max] + 1
    temp_votes[which_max] <- votes[which_max] / (party_seats[which_max] + 1)
  }

  party_seats
}

jofnunarsaeti <- function(seats, votes) {
  n_seats <- 9
  assigned_seats <- 0

  while (assigned_seats < n_seats) {
    perc_votes <- votes / sum(votes)
    too_low <- perc_votes < 0.05
    perc_votes[too_low] <- 0
    perc_seats <- seats / sum(seats)
    diff <- perc_votes - perc_seats
    which_max <- which.max(diff)
    seats[which_max] <- seats[which_max] + 1
    assigned_seats <- assigned_seats + 1
  }

  seats
}

gs4_auth()

electorate_byagearea <- read_csv("data/electorate_byagearea.csv")
maskina_aldur <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "maskina_aldur"
) |>
  select(-Ár, -Mánuður) |>
  pivot_longer(
    cols = -Flokkur,
    names_to = "aldur",
    values_to = "p_aldur"
  ) |>
  janitor::clean_names() |>
  mutate(
    p_aldur = p_aldur / sum(p_aldur),
    .by = flokkur
  )

maskina_kjordaemi <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "maskina_kjordaemi"
) |>
  select(-Ár, -Mánuður) |>
  pivot_longer(
    cols = -Flokkur,
    names_to = "kjordaemi",
    values_to = "p_kjordaemi"
  ) |>
  janitor::clean_names() |>
  mutate(
    p_kjordaemi = p_kjordaemi / sum(p_kjordaemi),
    .by = flokkur
  )

y_rep <- read_csv("data/y_rep_draws.csv") |>
  filter(dags == max(dags))

d <- y_rep |>
  mutate(
    value = value / sum(value),
    .by = c(.draw, .iteration, .chain)
  ) |>
  inner_join(
    maskina_kjordaemi,
    by = join_by(flokkur)
  ) |>
  inner_join(
    maskina_aldur,
    by = join_by(flokkur)
  ) |>
  inner_join(
    electorate_byagearea,
    by = join_by(kjordaemi, aldur)
  ) |>
  mutate(
    value_raw = value,
    value_kjosendur = value * n_kjosendur,
    value = value * p_aldur * p_kjordaemi * n_kjosendur
  )

seats_tibble <- tribble(
  ~kjordaemi, ~n_seats,
  "Reykjavík Suður", 9,
  "Reykjavík Norður", 9,
  "Suðvestur", 10,
  "Suður", 9,
  "Norðvestur", 8,
  "Norðaustur", 9
)

plot_dat <- d |>
  summarise(
    value = sum(value),
    value_raw = sum(value_raw),
    value_kjosendur = sum(value_kjosendur),
    .by = c(flokkur, .draw, kjordaemi)
  ) |>
  inner_join(
    seats_tibble,
    by = join_by(kjordaemi)
  ) |>
  mutate(
    seats = dhondt(value, n_seats),
    .by = c(.draw, kjordaemi)
  ) |>
  summarise(
    total_votes = sum(value),
    total_votes_raw = sum(value_raw),
    total_votes_kjosendur = sum(value_kjosendur),
    total_seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  mutate(
    seats = jofnunarsaeti(total_seats, total_votes),
    .by = c(.draw)
  ) |>
  summarise(
    mean_seats = mean(seats),
    mean_raw_seats = mean(total_seats),
    mean_votes = mean(total_votes),
    mean_raw_votes = mean(total_votes_raw),
    mean_kjosendur_votes = mean(total_votes_kjosendur),
    .by = flokkur
  ) |>
  mutate(
    p_votes = mean_votes / sum(mean_votes),
    p_raw_votes = mean_raw_votes / sum(mean_raw_votes),
    p_kjosendur_votes = mean_kjosendur_votes / sum(mean_kjosendur_votes)
  ) |>
  mutate(
    flokkur = fct_reorder(flokkur, p_votes)
  )

plot_dat |>
  ggplot(aes(p_votes, flokkur)) +
  geom_point(
    aes(
      x = p_votes,
      col = "Eftir vigtun",
      shape = "Eftir vigtun"
    ),
    size = 2.5
  ) +
  geom_point(
    aes(
      x = p_raw_votes,
      col = "Fyrir vigtun"
    ),
    size = 2.5
  ) +
  geom_point(
    aes(
      x = p_kjosendur_votes,
      col = "Kjósendur",
      shape = "Kjósendur"
    ),
    size = 2.5,
    position = position_jitter(height = 0.3, seed = 1)
  )


plot_dat |>
  mutate(
    flokkur = fct_reorder(flokkur, mean_seats)
  ) |>
  ggplot(aes(mean_seats, flokkur)) +
  geom_point(
    aes(
      x = mean_seats,
      col = "Eftir jöfnun"
    ),
    size = 3
  ) +
  geom_point(
    aes(
      x = mean_raw_seats,
      col = "Fyrir jöfnun"
    ),
    size = 3
  )


d |>
  summarise(
    value = sum(value),
    .by = c(flokkur, .draw, kjordaemi)
  ) |>
  inner_join(
    seats_tibble,
    by = join_by(kjordaemi)
  ) |>
  mutate(
    seats = dhondt(value, n_seats),
    .by = c(.draw, kjordaemi)
  ) |>
  mutate(
    p_seats = seats / sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  summarise(
    seats = mean(seats),
    p_seats = mean(p_seats),
    .by = c(flokkur, kjordaemi)
  ) |>
  mutate(
    flokkur = fct_reorder(flokkur, seats, sum)
  ) |>
  ggplot(aes(seats, flokkur, fill = kjordaemi)) +
  geom_col()
