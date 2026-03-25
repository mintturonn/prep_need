library(tidyverse)

eversex_props <- tribble(
  ~population,    ~`15-17`, ~`18`, ~`19`,
  "Female", 0.258,   0.566, 0.699,
  "Male",   0.250,   0.579, 0.681
)

# Reshape to long format for plotting
eversex_long <- eversex_props |>
  pivot_longer(
    cols = c(`15-17`, `18`, `19`),
    names_to = "age_group",
    values_to = "prop"
  )


ggplot(eversex_long, aes(x = age_group, y = prop, fill = population, group = population)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  labs(
    x = "Age group",
    y = "Ever had sex" ) +
  theme_minimal(base_size = 12)