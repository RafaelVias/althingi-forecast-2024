library(tidyverse)
library(googlesheets4)
library(here)

# read data
gs4_auth()
sheets_data <- read_sheet("https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4/edit?gid=0#gid=0")

out <- sheets_data |>
  janitor::clean_names() |>
  mutate(
    date = clock::date_build(ar, manudur),
    n = hlutfall * fjoldi_alls
  ) |>
  select(
    date,
    fyrirtaeki,
    flokkur,
    n
  )

out |>
  write_csv(here("data", "sheets_data.csv"))
