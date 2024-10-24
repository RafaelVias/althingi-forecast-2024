library(tidyverse)
library(ggiraph)
library(metill)
library(patchwork)
library(here)
library(arrow)
library(glue)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill())

today_date <- Sys.Date()
vote_date <- clock::date_build(2024, 11, 30)
days_until_vote <- as.numeric(vote_date - today_date)

caption <- str_c(
  "Samantektin byggir á fylgiskönnunum Félagsvísindastofnunar, Gallup, Maskínu og Prósents ",
  "ásamt niðurstöðum kosninga og kjörsókn 2021", "\n",
  "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni og Rafael Daniel Vias"
)

colors <- tribble(
  ~flokkur, ~litur,
  "Sjálfstæðisflokkurinn", "#377eb8",
  "Framsóknarflokkurinn", "#41ab5d",
  "Samfylkingin", "#e41a1c",
  "Vinstri Græn", "#00441b",
  "Viðreisn", "#ff7d14",
  "Píratar", "#984ea3",
  "Miðflokkurinn", "#08306b",
  "Flokkur Fólksins", "#FBB829",
  "Sósíalistaflokkurinn", "#67000d",
  "Annað", "grey50"
)

coverage_data <- read_parquet(here("data", "y_rep_draws.parquet")) |>
  reframe(
    mean = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(dags, flokkur)
  ) |>
  inner_join(
    colors
  )

coverage_data |>
  filter(
    dags == max(dags)
  ) |>
  mutate(
    flokkur = fct_reorder(flokkur, mean)
  ) |>
  ggplot(aes(
    y = flokkur,
    color = litur,
    group = paste(flokkur, coverage)
  )) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      alpha = -coverage
    ),
    linewidth = 5
  ) +
  geom_segment(
    aes(
      x = mean,
      xend = mean,
      y = as.integer(flokkur) - 0.4,
      yend = as.integer(flokkur) + 0.4
    ),
    linewidth = 1.
  ) +
  scale_color_identity() +
  scale_x_continuous(
    labels = label_percent(),
    limits = c(0, NA),
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 0.30
    )
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_alpha_continuous(
    range = c(0, 0.2)
  ) +
  coord_fixed(
    ratio = 0.01
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = glue("Fylgispá þegar {days_until_vote} dagar eru til kosninga"),
    subtitle = "Línustrik tákna miðgildi spár og hver kassi inniheldur 5% af niðurstöðum spár",
    caption = caption
  )
