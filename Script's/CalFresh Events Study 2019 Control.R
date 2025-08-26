library(pacman)
p_load(data.table,tidyverse,tidylog,janitor,readxl,tinytex,
       stringr,writexl,tidyr,dplyr,broom,stats,fixest,ggplot2)

# Load the cleaned CalFresh dataset
cf <- read_csv("cleaned_calfresh_annual (1).csv")

# Compute per capita enrollment and clean county names
cf <- cf %>%
  mutate(
    per_capita_enrollment = (`Age 18-59 CalFresh`) / `Total Population`,
    county = str_to_title(County),
    event_time = Year - 2019
  )


# Drop years outside desired window (optional)
cf <- cf %>% filter(event_time >= -5 & event_time <= 2)

# Convert event_time to factor, drop 0 (event year) as reference
cf$event_time <- factor(cf$event_time)
cf$event_time <- relevel(cf$event_time, ref = "0")

# Run fixed effects regression: outcome ~ event time dummies + county FE
event_model <- feols(
  per_capita_enrollment ~ i(event_time, ref = "0") | county,
  data = cf
)

# Show results
summary(event_model)

# Extract and plot coefficients
event_model <- feols(
  per_capita_enrollment ~ i(event_time, ref = 0) | county,
  data = cf
)

par(cex=0.6)

# Plot results
iplot(
  event_model,
  main = "Event Study: CalFresh Enrollment Ages 18-59 Around 2019",
  xlab = "Years Relative to 2019",
  ylab = "Effect on Per Capita Enrollment",
  ref.line = TRUE
)
