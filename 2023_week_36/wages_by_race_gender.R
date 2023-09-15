library(tidyverse)
library(gghighlight)
library(showtext)

# Add Google font 'Poppins'
font_add_google("Poppins", "poppins")

wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')

# Filter and preprocess data
wages_race_gender <- wages %>% 
  filter(str_detect(facet, "^demographics: .* (female|male)")) %>% 
  mutate(facet = str_replace(facet, "^demographics: ", "")) %>% 
  separate(facet, into = c("race", "gender"), sep = " ") %>% 
  select(year, wage, race, gender)

# Rename factor levels for 'gender' and 'race' columns
wages_race_gender$gender <- factor(wages_race_gender$gender, levels = c("male", "female"), labels = c("Men", "Women"))
wages_race_gender$race <- factor(wages_race_gender$race, levels = c("white", "black", "hispanic"), labels = c("White", "Black", "Hispanic"))

ggplot(wages_race_gender, aes(x = year, y = wage, color = race)) +
  geom_line(linewidth = .8) +
  facet_grid(. ~ gender) +
  labs(
    x = NULL, y = NULL,
    title = "Women's wages are rising, but not equally.",
    subtitle = "Mean hourly wages in nominal dollars",
    caption = "Source: Union Membership and Coverage Database \nGraphics: Thomas Shaw"
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, by = 10),
    labels = glue::glue("{seq(0, 40, by = 10)}$")
  ) +
  rcartocolor::scale_color_carto_d(
    palette = "Vivid",
    name = "Race"
  ) +
  theme_minimal(
    base_family = "poppins",
    base_size = 18
  ) +
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
    panel.grid.major.y = element_line(color = "grey90", linewidth = .4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(2.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    plot.margin = margin(30, 35, 30, 30)
  )