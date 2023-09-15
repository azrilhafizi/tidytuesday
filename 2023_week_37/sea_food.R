library(tidyverse)
library(showtext)

font_add_google("Space Grotesk", "space")

showtext_auto()

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')

sea_food <- all_countries %>% 
  inner_join(country_regions, by = "country_iso3") %>% 
  filter(region_name == "South-eastern Asia" & Category == "Food provision") %>% 
  mutate(country_name = str_replace(country_name, "Lao People's Democratic Republic", "Laos")) %>% 
  group_by(country_name) %>% 
  summarise(total_min = sum(hoursPerDayCombined) * 60)

sea_food %>% 
  arrange(total_min) %>% 
  mutate(country_name = factor(country_name, country_name)) %>% 
  ggplot(aes(x = country_name, y = total_min)) +
  geom_segment(aes(x = country_name, xend = country_name, y = 0, yend = total_min), color = "grey") +
  geom_point(size=4, color="#006400") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 150, by = 50),
    labels = glue::glue("{seq(0, 150, by = 50)} mins")
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Minutes Spent on Food Provision",
    subtitle = "Understanding the daily commitment to food preparation in Southeast Asia",
    caption = "Source: The Human Chronome Project  \nGraphics: Thomas Shaw"
  ) +
  theme_minimal(base_family = "space") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 35, color = "black", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 20, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(color = "grey25", size = 10, margin = margin(t = 5)),
    plot.caption.position = "plot",
    strip.text = element_text(size = 20),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, margin = margin(l = 10, r = 7)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(.5, "lines"),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(2.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    plot.margin = margin(30, 35, 30, 30)
  )