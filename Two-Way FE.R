# Load packages
library(pacman)
p_load(tidyverse, fixest, ggplot2, readr)

# Load data
df <- read_csv("Regression_Ready.csv")

# Convert CalFresh enrollment into a more readable scale (e.g., per 1,000 people)
df <- df %>%
  mutate(calfresh_scaled = calfresh_per_capita * 1000)

# Fit a two-way fixed effects model
model <- feols(calfresh_scaled ~ minimum_wage | county + year, data = df)

# Predict fitted values for the plot
df$predicted <- predict(model)

# Plot
ggplot(df, aes(x = minimum_wage, y = calfresh_scaled)) +
  geom_point(alpha = 0.3, color = "gray") +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange", fill = "orange", size = 1) +
  labs(
    title = "CalFresh Enrollment vs. Minimum Wage",
    subtitle = "Two-Way Fixed Effects Model (2016–2024)",
    x = "Minimum Wage (USD)",
    y = "Avg. CalFresh Enrollment (per 1,000 residents)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10)
  )