library(tidyverse)
library(gghighlight)
library(showtext)

# Add Google font 'Poppins'
font_add_google("Poppins", "poppins")

# Automatically enable text rendering using the added font
showtext_auto()

# Load data from URLs
fair_use_cases <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv')
fair_use_findings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_findings.csv')

# Prepare fair_use_cases data
case_by_cat <- fair_use_cases %>%
  filter(year >= 1990) %>%
  mutate(categories = gsub(",", ";", categories)) %>%
  separate(categories, into = paste0("tag_", 1:5), sep = ";", extra = "drop") %>%
  mutate(across(starts_with("tag_"), str_trim, side = "both")) %>%
  mutate(across(starts_with("tag_"), tolower)) %>%
  mutate(across(starts_with("tag_"), ~ ifelse(grepl("circuit", .), NA, .))) %>%
  select(case, year, tag_1, tag_2, tag_3, tag_4, tag_5)

# Mapping to clean up tag categories
tag_mapping <- c(
  "educational/scholarship/research" = "education/scholarship/research",
  "education/research/scholarship" = "education/scholarship/research",
  "films/audiovisual" = "film/audiovisual",
  "photography" = "photograph",
  "news reporting photograph" = "news reporting",
  "internet/digitization" = "internet",
  "parody/satire" = "parody"
)

# Mapping for new tag categories
new_tags <- c(
  "education/scholarship/research" = "Academia",
  "film/audiovisual" = "Film/Audiovisual",
  "internet" = "Internet",
  "music" = "Music",
  "news reporting" = "News",
  "others" = "Others",
  "painting/drawing/graphic" = "Painting/Graphic",
  "parody" = "Parody",
  "photograph" = "Photography",
  "review/commentary" = "Review/Commentary",
  "textual work" = "Textual Work"
)

# Pivot the data to long format
data_long <- case_by_cat %>%
  pivot_longer(cols = starts_with("tag_"), names_to = NULL, values_drop_na = TRUE) %>%
  mutate(category = str_replace_all(value, tag_mapping)) %>%
  filter(category != "") %>%
  group_by(category) %>%
  mutate(total_cases = n()) %>%
  mutate(category = ifelse(total_cases <= 15, "others", category)) %>%
  mutate(category = str_replace_all(category, new_tags)) %>%
  select(year, category)

# Calculate tag counts and percentage changes
tag_counts <- data_long %>%
  group_by(year, category) %>%
  summarise(case_count = n()) %>%
  arrange(year) %>%
  arrange(category, year) %>%
  group_by(category) %>%
  mutate(percentage_change = (case_count / lag(case_count, default = first(case_count))) - 1) %>%
  ungroup()

# Create the ggplot visualization
ggplot(tag_counts, aes(x = year, y = percentage_change, color = category)) +
  geom_point() +
  geom_line(linewidth = .8, alpha = .5) +
  gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey80", size = .5)
  ) +
  geom_hline(yintercept = 0, color = "grey30", size = .8) +
  geom_area(alpha = .2) +
  geom_line(alpha = 1.2) +
  geom_point(alpha = 1.8) +
  facet_wrap(~ category, ncol = 4, scales = "free_x") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(
    breaks = seq(-1, 4, by = 1),
    labels = glue::glue("{seq(-1, 4, by = 1)}%")
  ) +
  rcartocolor::scale_color_carto_d(
    palette = "Prism",
    guide = "none"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Year-over-year percent change in number of fair use cases",
    caption = "Source: U.S. Copyright Office Fair Use Index \nGraphics: Thomas Shaw"
  ) +
  theme_minimal(
    base_family = "poppins",
    base_size = 14
  ) +
  theme(
    plot.title = element_text(size = 22, margin = margin(b = 20)),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey25", size = 10, margin = margin(t = 0)),
    plot.caption.position = "plot",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, margin = margin(l = 10, r = 7)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(.5, "lines"),
    strip.text = element_text(size = 15, face = "bold", margin = margin(b = 5)),
    panel.grid.major.y = element_line(color = "grey90", linewidth = .4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(2.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    plot.margin = margin(20, 35, 20, 20)
  )