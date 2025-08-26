library(pacman)
p_load(data.table,tidyverse,tidylog,janitor,readxl,tinytex,
       stringr,writexl,tidyr,dplyr,broom,stats,fixest,ggplot2,tigris,
       sf,scales)

# Step 1: Read in the new CalFresh dataset
cf <- read.csv("cleaned_calfresh_annual (1).csv")

# Step 2: Clean and calculate per capita enrollment as a percentage
cf <- cf %>%
  mutate(
    County = str_to_title(County),
    per_capita_enrollment = (Age.18.59.CalFresh / Total.Population) * 100
  )

# Step 3: Load California county shapefile
options(tigris_use_cache = TRUE)
ca_counties <- counties(state = "CA", cb = TRUE, class = "sf")
ca_counties$NAME <- str_to_lower(ca_counties$NAME)

### FUNCTION TO GENERATE HEATMAPS
make_heatmap <- function(year, show_legend = FALSE, filename = "heatmap.png") {
  data_filtered <- cf %>%
    filter(Year == year) %>%
    select(County, per_capita_enrollment) %>%
    mutate(County = str_to_lower(County))
  
  map_data <- left_join(ca_counties, data_filtered, by = c("NAME" = "County"))
  
  heatmap <- ggplot(map_data) +
    geom_sf(aes(fill = per_capita_enrollment), color = "white") +
    scale_fill_viridis_c(
      option = "plasma",
      name = if (show_legend) "CalFresh Enrollment (%)" else NULL,
      labels = label_percent(scale = 1),
      limits = c(0, 15),
      na.value = "grey90"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.caption = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
  ggsave(filename, heatmap, width = 6, height = 6, dpi = 300)
}

# Generate and save maps
make_heatmap(2016, show_legend = FALSE, filename = "calfresh_heatmap_2016.png")
make_heatmap(2021, show_legend = FALSE, filename = "calfresh_heatmap_2021.png")
make_heatmap(2023, show_legend = TRUE, filename = "calfresh_heatmap_2023.png")