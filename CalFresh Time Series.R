library(pacman)
p_load(data.table,tidyverse,tidylog,janitor,readxl,tinytex,
       stringr,writexl,tidyr,dplyr,broom,stats,fixest,ggplot2)

# Load the cleaned CalFresh dataset
cf <- read_csv("cleaned_calfresh_annual (1).csv")


# Step 1: Add per capita variable
cf <- cf %>%
  mutate(
    per_capita_enrollment = (`Age 18-59 CalFresh`) / `Total Population`
  )

# Step 2a: Total enrollment across all counties
total_ts <- cf %>%
  group_by(Year) %>%
  summarise(total_enrollment = sum(`Age 18-59 CalFresh`, na.rm = TRUE))

# Step 2b: Average per capita enrollment across counties
per_capita_ts <- cf %>%
  group_by(Year) %>%
  summarise(avg_per_capita = mean(per_capita_enrollment, na.rm = TRUE))


# Step 3b: Plot average per capita enrollment over time
ggplot(per_capita_ts, aes(x = Year, y = avg_per_capita)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue") +
  labs(title = "Avg Per Capita CalFresh Enrollment by County",
       x = "Year", y = "Per Capita Enrollment") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 10))