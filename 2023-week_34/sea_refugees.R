library(tidyverse)
library(ggsankey)
library(showtext)
library(ggtext)

font_add_google("Space Grotesk", "space")

showtext_auto()

population <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

region <- readr::read_csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')

# Prepare refugee data for South-eastern Asia
sea_ref <- population %>% 
  inner_join(region, by = c("coo_iso" = "alpha-3")) %>% 
  select(year, coo_name, refugees, `sub-region`) %>% 
  filter(`sub-region` == "South-eastern Asia") %>% 
  group_by(coo_name, year) %>% 
  summarise(
    total = sum(refugees)
  ) %>% 
  ungroup()

# Calculate total refugees for all years for each country
total_all_years <- sea_ref %>% 
  group_by(coo_name) %>% 
  summarise(grand_total = sum(total))

# Identify top 2 countries by total refugees
top_2_country <- sea_ref %>% 
  group_by(coo_name) %>% 
  summarise(grand_total = sum(total)) %>% 
  arrange(desc(grand_total)) %>%
  head(2)

# Create a Sankey plot
sea_ref %>% 
  ggplot() +
  geom_sankey_bump(aes(x = year, node = coo_name, fill = if_else(coo_name %in% top_2_country$coo_name, coo_name, NA), value = total, color = after_scale(colorspace::lighten(fill, 0.4))), linewidth = 0.3, type = "alluvial", space = 1e3, alpha = 0.9) +
  annotate("text", x = 2022.2, y = 1.5e6, label = "Total\nrefugees as\n2022\n↓", hjust = 0, vjust = 1, color = "grey30") +
  geom_text(data = top_2_country, aes(x = 2022.2, y = c(8e5, 2e5), label = scales::number(grand_total, accuracy = 0.1, scale_cut = scales::cut_short_scale())), hjust = 0, color = c("#E74C3C", "#3498DB")) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_fill_manual(values = c("#E74C3C", "#3498DB"), na.value = "grey60") +
  scale_color_manual(values = c("#E74C3C", "#3498DB"), na.value = "grey60") +
  scale_size_continuous(range = c(4, 6)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  annotate(geom = "richtext", x = 2010, y = 2e6, label = "Between 2010 to 2022, <span style='color:#E74C3C;'>Myanmar</span> and <span style='color:#3498DB;'>Vietnam</span><br> together constitued nearly <b>97%</b> <br>of the total refugee population<br>in Southeast Asia.", size = 5.5, hjust = 0, color = "#181716", label.color = NA) +
  labs(
    caption = "Souce: UNHCR Refugee Population Statistics Database \n Graphics: Thomas Shaw"
  ) +
  theme_minimal(base_family = "space") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFFFFE", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12, margin = margin(5, 0, 0, 0), color = "#393433"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_line(color = "grey70"),
    plot.margin = margin(80, 75, 10, 35),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  )