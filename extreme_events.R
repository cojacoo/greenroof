# Load necessary libraries
## Install and load libraries ####
if (!require("dplyr")) install.packages("dplyr")
if (!require("plotly")) install.packages("plotly")
if (!require("zoo")) install.packages("zoo")

library(dplyr)
library(plotly)
library(zoo)

## Set to your working directory if needed
#setwd("./")

# Read the data from the CSV file
#data <- read.csv("3653897.csv")
data <- read.csv('https://github.com/cojacoo/greenroof/raw/refs/heads/main/3653897.csv')

# Convert DATE to a date format and set as index
data$DATE <- as.Date(data$DATE)
rownames(data) <- data$DATE
data$year <- year(data$DATE)

# Convert precipitation from inches to millimeters
data$PRCPmm <- data$PRCP * 25.4

# Simple scatter plot of precipitation
plot_ly(data, x = ~DATE, y = ~PRCPmm, type = "scatter", mode = "markers") %>%
  layout(template = "none")

# Histogram
plot_ly(data, x = ~PRCPmm, type = "histogram")%>%
  layout(template = "none")

# 99th percentile
ninety_ninth_percentile <- quantile(data$PRCPmm, 0.99)
cat("99th percentile of precipitation:", ninety_ninth_percentile, "\n")

# Scatter plot of extreme precipitation events
data_extreme <- data %>%
  filter(PRCPmm > ninety_ninth_percentile)

plot_ly(data_extreme, x = ~DATE, y = ~PRCPmm, type = "scatter", mode = "markers")%>%
  layout(template = "none")

# Time series of counts of strongest rains per year
data$year <- as.integer(format.Date(data$DATE, "%Y"))
datax <- data %>%
  filter(PRCPmm > quantile(PRCPmm, 0.99)) %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  mutate(r_year = year - min(year))

plot_ly(datax, x = ~year, y = ~count, type = "scatter", mode = "markers") %>%
  layout(template = "none")

# Start with rank statistics - sorted data
sorted_data <- data %>%
  arrange(PRCPmm)

plot_ly(sorted_data, y = ~PRCPmm, type = "scatter", mode = "markers") %>%
  layout(template = "none")

# Sorted data with undercut probability
fig <- plot_ly(x = (1:nrow(sorted_data))/(nrow(sorted_data) + 1), y = sorted_data$PRCPmm, type = "scatter", mode = "markers")
fig$layout$xaxis$title <- "P_undercut"
fig$layout$yaxis$title <- "Precip (mm/day)"
fig

# Plotting positions (full time series)
annual_max <- data %>%
  group_by(year) %>%
  summarize(Precip = max(PRCPmm)) %>%
  arrange(Precip) %>%
  mutate(Probability = (1:n())/n(), Annuality = 1/(1 - Probability))

# Plot with color gradient
plot_ly(annual_max, x = ~Probability, y = ~Precip, type = "scatter", mode = "markers", color = ~year) %>%
  layout(template = "none")

plot_ly(annual_max, x = ~Annuality, y = ~Precip, type = "scatter", mode = "markers", color = ~year) %>%
  layout(template = "none", xaxis = list(type = "log"))


# Plotting positions (last 30 years)
annual_max30 <- data %>%
  filter(year > 1995) %>%
  group_by(year) %>%
  summarize(Precip = max(PRCPmm)) %>%
  arrange(Precip) %>%
  mutate(Probability = (1:n())/n(), Annuality = 1/(1 - Probability))
  
# Plot with color gradient
plot_ly(annual_max30, x = ~Probability, y = ~Precip, type = "scatter", mode = "markers", color = ~year) %>%
  layout(template = "none")

# Rolling 25-year means of annualities
annual_max_sorted <- annual_max[order(annual_max$year), ]  # Sort by year
plot_ly(x = annual_max_sorted$year[25:145], y = rollapply(annual_max_sorted$Annuality, 25, mean, na.rm = TRUE), type = "scatter", mode = "markers")

