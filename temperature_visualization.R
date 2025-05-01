# Install and load required libraries
if (!require(fontawesome)) install.packages("fontawesome")
library(tidyverse)
library(janitor)
library(ggrepel)
library(ggtext)
library(stringr)
library(scales)
library(MetBrewer)
library(lubridate)
library(ggh4x) 
library(showtext)
library(sysfonts)

# Create the GitHub icon
github_icon <- fontawesome::fa("github", fill = "#474747", height = "11px")

# Load fonts
font_add_google("Ubuntu", "Ubuntu")
font_add_google("Chakra Petch", "Chakra Petch")
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "Font-Awesome-6-Brands-Regular-400.ttf")
showtext::showtext_auto()

# Define temperature label formatting function
celsius_label <- function(c_temps, digits = 1) {
  c_temps_rounded <- round(c_temps, digits = digits)
  formatted_labels <- paste0(c_temps_rounded, "°C")
  return(formatted_labels)
}

# Set custom theme
theme_set(
    theme_minimal() +
    theme(
    axis.line.y.left = element_line(color = '#474747', linewidth = .3),
    axis.ticks.y= element_line(color = '#474747', linewidth = .3),
    panel.grid = element_line(linewidth = .3, color = 'grey90'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.background = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(family = "Ubuntu", size = 16, face = 'bold'),
    plot.caption = element_text(size = 6, color = '#474747',margin = margin(10,0,0,0)),
    plot.subtitle = element_text(size = 10,lineheight = 1.15, margin = margin(0,0,20,0)),
    axis.title.x = element_markdown(family = "Ubuntu", hjust = .5, size = 9),
    axis.title.y = element_markdown(family = "Ubuntu", hjust = .5, size = 8),
    axis.text = element_markdown(family = "Chakra Petch", hjust = .5, size = 8, color = "#474747"),
    legend.position = "top",
    text = element_text(family = "Ubuntu"),
    plot.margin = margin(25, 25, 25, 25))
)

# Process temperature data
data2plot <- 
  read_csv('city_temperature.csv') |>
  clean_names() |>
  filter((country == 'Japan' & city == 'Tokyo') |
         country %in% c('The Netherlands', 'South Africa')) |>
  filter(avg_temperature > 0) |>
  mutate(
    month_abbr = month(month, label = TRUE, abbr = TRUE),
    month_abbr = factor(month_abbr, levels = month(1:12, label = TRUE, abbr = TRUE)),
    avg_temperature = (avg_temperature - 32) * 5 / 9
  )

# Calculate month-year bounds
month_year_bounds <- data2plot |>
  group_by(city, month) |>
  summarise(
    min_year_for_month = min(year, na.rm = TRUE),
    max_year_for_month = max(year, na.rm = TRUE),
    .groups = 'drop'
  )

# Join bounds and filter data
filtered_data <- data2plot |>
  left_join(month_year_bounds, by = c("city", "month")) |>
  filter(year == min_year_for_month | year == max_year_for_month)

# Calculate summary statistics
summary_table <- filtered_data |>
  group_by(city, year, month, min_year_for_month, max_year_for_month) |>
  summarise(
    monthly_avg_temp = mean(avg_temperature, na.rm = TRUE),
    monthly_sd_temp = sd(avg_temperature, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(
    record_type = case_when(
      min_year_for_month == max_year_for_month ~ "Single Year Record for Month",
      year == min_year_for_month ~ "Oldest Record for Month",
      year == max_year_for_month ~ "Newest Record for Month",
      TRUE ~ NA
    )
  ) |>
  select(city, year, month, monthly_avg_temp, monthly_sd_temp, record_type) |>
  arrange(city, month, year) |>
  mutate(
    month_abbr = month(month, label = TRUE, abbr = TRUE),
    month_abbr = factor(month_abbr, levels = month(1:12, label = TRUE, abbr = TRUE)))

# Calculate temperature differences
delta_data <- summary_table |>
  select(city, month_abbr, record_type, monthly_avg_temp) |>
  pivot_wider(
    names_from = record_type,
    values_from = monthly_avg_temp
  ) |>
  mutate(
    temp_diff = `Newest Record for Month` - `Oldest Record for Month`,
    arrow = case_when(
      temp_diff > 0 ~ "▲",
      temp_diff < 0 ~ "▼",
      TRUE          ~ " "
    ),
    direction = case_when(
      temp_diff > 0 ~ 1,
      temp_diff < 0 ~ -1,
      TRUE          ~ 0
    ),
    label_color = case_when(
      temp_diff > 0 ~ "red",
      temp_diff < 0 ~ "dodgerblue3",
      TRUE          ~ "grey50"
    ),
    label = str_c(arrow,' ',celsius_label(temp_diff))
  )

# Create visualization
data2plot |>
  ggplot(aes(x = month_abbr, y = avg_temperature)) +
  geom_point(aes(color = year), position = 'jitter', size = .1) +
  # White shadow/border
  geom_segment(
    data = summary_table,
    aes(
      x     = as.numeric(month_abbr) - 0.4,
      xend  = as.numeric(month_abbr) + 0.4,
      y     = monthly_avg_temp,
      yend  = monthly_avg_temp,
      group = year
    ), 
    color = 'white',
    linewidth = 2
  ) +  
  geom_segment(
    data = summary_table,
    aes(
      x     = as.numeric(month_abbr) - 0.4,
      xend  = as.numeric(month_abbr) + 0.4,
      y     = monthly_avg_temp,
      yend  = monthly_avg_temp,
      color = year
    )
  ) +
  geom_label(
    data = delta_data,
    y = 35,
    aes(label = label),
    color = delta_data$label_color,
    size = 2.5,
    show.legend = FALSE,
    fill = 'white',
    label.padding = unit(0.1, "lines")
  ) + 
  facet_wrap(~city, nrow = 3, scales = 'free') +
  scale_color_gradientn(
    colours = MetBrewer::MetPalettes$Cassatt1[[1]],
    breaks = c(1995,2020)
  ) +
  scale_y_continuous(
    label = celsius_label, 
    limits = c(-15,40),
    breaks = c(-15, 0, 15, 30, 40),
    expand = c(0,0,0,0)) +
  theme(
    strip.text = element_text(face = 'bold'),
    legend.position = 'bottom',
    panel.background = element_rect(fill = 'grey97', color = NA)
  ) +
  labs(
    x = NULL,
    y = "Daily Average Temperature",
    color = NULL,
    title = 'A Tale of Three Climates: Amsterdam, Tokyo, and Cape Town',
    subtitle = str_wrap("Each point represents the daily average temperature recorded in 1995 and 2020, with vertical lines indicating the respective monthly means for both years. While daily fluctuations are evident, the broader trend suggests a noticeable shift in temperature patterns over time.", width = 160),
    caption = "<span style='font-family:\"Font Awesome 6 Brands\"'>&#xf09b;</span> umutevren <span style='font-family:\"Font Awesome 6 Brands\"'>&#xf08c;</span> umutevren  |  **Data:** Kaggle Daily Temperature of Major Cities"
  ) +
  theme(
    plot.caption = element_markdown(size = 8, color = '#474747', margin = margin(10,0,0,0), family = "Ubuntu"),
    plot.caption.position = "plot"
  ) +
  guides(
    color = guide_colorbar(
      barwidth = 6,
      barheight = .5
    )
  )

# Save the plot as PDF
ggsave("/Users/umutevren/kaggletemp/temperature_visualization.pdf", 
       width = 12, 
       height = 10,
       device = cairo_pdf)

