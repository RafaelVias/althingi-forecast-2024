library(tidyverse)
library(ggiraph)
library(metill)
library(patchwork)
library(here)

theme_set(theme_metill())

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

# read data
gallup_data <- read_csv(here("data", "gallup_data.csv"))
maskina_data <- read_csv(here("data", "maskina_data.csv"))
prosent_data <- read_csv(here("data", "prosent_data.csv"))

# combine data
poll_data <- bind_rows(
  maskina_data,
  prosent_data,
  gallup_data
) |>
  mutate(
    flokkur = if_else(flokkur == "Lýðræðisflokkurinn", "Annað", flokkur)
  ) |>
  mutate(
    p = n / sum(n),
    .by = c(date, fyrirtaeki)
  ) |> 
  select(
    dags = date,
    fyrirtaeki,
    flokkur,
    p_poll = p
  )

d <- read_csv("data/y_rep_draws.csv") |> 
  mutate(
    value = value / sum(value),
    .by = c(.iteration, .chain, .draw, dags)
  ) |> 
  summarise(
    mean = mean(value),
    .by = c(dags, flokkur)
  ) |> 
  inner_join(
    colors
  ) |> 
  inner_join(
    poll_data
  )


p1 <- d |> 
  filter(dags == max(dags)) |> 
  mutate(
    flokkur_ordered = fct_reorder(flokkur, mean)
  ) |> 
  ggplot(aes(mean, flokkur_ordered, color = litur, data_id = flokkur)) +
  geom_text_interactive(
    aes(label = flokkur, x = 0),
    hjust = 1,
    nudge_x = -0.005,
    size = 5
  ) +
  geom_segment_interactive(
    aes(xend = 0, yend = flokkur_ordered),
    alpha = 0.3,
    linewidth = 0.6
  ) +
  geom_point_interactive(
    size = 2.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.3, by = 0.05),
    guide = ggh4x::guide_axis_truncated(),
    labels = label_percent()
  ) +
  scale_colour_identity() +
  coord_cartesian(clip = "off", xlim = c(-0.035, NA)) +
  theme(
    plot.margin = margin(5, 15, 5, 15),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Staðan í nýjustu könnunum"
  )

p2 <- d |> 
  ggplot(aes(dags, mean, colour = litur, data_id = flokkur)) +
  geom_smooth_interactive(
    method = "loess",
    span = 0.4,
    se = 0
  ) +
  geom_point_interactive(
    aes(y = p_poll)
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.3, by = 0.05),
    guide = ggh4x::guide_axis_truncated(),
    labels = label_percent()
  ) +
  scale_colour_identity() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fylgisþróun"
  )


p <- wrap_plots(
  p1, p2,
  ncol = 1,
  heights = c(0.7, 1)
)

girafe(
  ggobj = p,
  width_svg = 11,
  height_svg = 0.9 * 11,
  bg = "transparent",
  options = list(
    opts_tooltip(
      opacity = 0.8,
      use_fill = TRUE,
      use_stroke = FALSE,
      css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"
    ),
    opts_hover(css = ""),
    opts_hover_inv(css = "opacity:0.05"),
    opts_toolbar(saveaspng = TRUE),
    opts_zoom(max = 1)
  )
)
