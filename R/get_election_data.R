library(tidyverse)


d <- tribble(
  ~date, ~flokkur, ~p,
  "2021-09-25", "Samfylkingin", 0.099,
  "2021-09-25", "Sjálfstæðisflokkurinn", 0.244,
  "2021-09-25", "Miðflokkurinn", 0.054,
  "2021-09-25", "Framsóknarflokkurinn", 0.173,
  "2021-09-25", "Vinstri Græn", 0.126,
  "2021-09-25", "Flokkur Fólksins", 0.088,
  "2021-09-25", "Viðreisn", 0.083,
  "2021-09-25", "Píratar", 0.086
)

out <- d |>
  bind_rows(
    tibble(
      date = "2021-09-25",
      flokkur = "Annað",
      p = 1 - sum(d$p)
    )
  ) |>
  mutate(
    n = p * 199730,
    fyrirtaeki = "Kosning"
  ) |>
  select(-p)

write_csv(out, here("data", "election_data.csv"))
