# Load libraries
library(pacman)
p_load(data.table,tidyverse,tidylog,janitor,readxl,tinytex,
       stringr,writexl,tidyr,dplyr,broom,stats,fixest,modelsummary,texreg)
# Step 1: Load the dataset
# Load the dataset
df <- read_csv("Regression_Ready.csv")


# Run fixed effects model with clustered SEs
model <- feols(
  calfresh_per_capita ~ minimum_wage + median_income + total_population | county + year,
  data = df,
  cluster = ~county
)

# Extract R² and fit stats from model
fit_stats <- fitstat(model, type = c("r2", "ar2", "wr2", "war2"))

# Manually build GOF stats, rounding to 4 decimals
gof_values <- c(
  nobs = model$nobs,
  county_fe = length(unique(df$county)),
  year_fe = length(unique(df$year)),
  r2 = round(as.numeric(fit_stats["r2"]), 4),
  adjr2 = round(as.numeric(fit_stats["ar2"]), 4),
  r2_within = round(as.numeric(fit_stats["wr2"]), 4),
  adjr2_within = round(as.numeric(fit_stats["war2"]), 4)
)

# Export table to LaTeX
texreg(
  model,
  file = "calfresh_regression_table.tex",
  custom.coef.names = c("Minimum Wage", "Median Income", "Population"),
  digits = 9,
  stars = c(0.01, 0.05, 0.1),
  caption = "Fixed Effects Regression on CalFresh Enrollment",
  label = "tab:calfresh_fe",
  override.gof = gof_values,
  override.gof.names = c(
    "Num. obs.",
    "County FE",
    "Year FE",
    "$R^2$ (overall)",
    "Adj. $R^2$ (overall)",
    "$R^2$ (within)",
    "Adj. $R^2$ (within)"
  ),
  include.aic = FALSE,
  include.bic = FALSE,
  include.rmse = FALSE,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.fstatistic = FALSE
)
input{calfresh_regression_table.tex}