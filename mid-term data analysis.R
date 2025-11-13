# Load required packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
.
library(ggcorrplot)

# 0. Load and pivot dataset

data <- openaq_location_6928_measurments %>%
  pivot_wider(names_from = parameter, values_from = value)


# 1. Handle Missing Values

colSums(is.na(data))

data <- data %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character),
                ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))


# 2. Remove Duplicates

data <- distinct(data)

# 3. Rename Columns

data <- data %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  rename_with(~ tolower(.x))

# 4. Convert Data Types

data <- data %>%
  mutate(across(contains("date") | contains("time") | contains("utc"), ymd_hms, quiet = TRUE)) %>%
  mutate(across(where(is.character), as.factor))

# 5. Handle Outliers (IQR)

pollutant_cols <- c("pm25", "pm10", "no2", "co", "so2")
for (col in pollutant_cols) {
  if (col %in% names(data)) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    data <- data %>%
      filter(data[[col]] > (Q1 - 1.5 * IQR_val) & data[[col]] < (Q3 + 1.5 * IQR_val))
  }
}
# 6. Descriptive Statistics

summary_stats <- data %>%
  summarise(across(all_of(pollutant_cols),
                   list(mean = mean,
                        median = median,
                        variance = var,
                        sd = sd),
                   na.rm = TRUE))
print(summary_stats)

# 7. Correlation Matrix

pollutants <- pollutant_cols[pollutant_cols %in% names(data)]
corr_matrix <- cor(data[, pollutants], use = "complete.obs")
print(corr_matrix)

# 8. Visual Analysis


# Histogram
ggplot(data, aes(x = pm25)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of PM2.5", x = "PM2.5", y = "Frequency")

# Boxplot by location
if ("location_name" %in% names(data)) {
  ggplot(data, aes(x = location_name, y = pm25, fill = location_name)) +
    geom_boxplot() +
    labs(title = "PM2.5 Variation Across Locations") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

if ("utc" %in% names(data)) {
  # Convert utc to POSIXct if needed
  data <- data %>%
    filter(!is.na(pm25)) %>%          # remove missing PM2.5
    mutate(utc = ymd_hms(utc))        # convert to datetime
  
  # Plot the time series
  print(
    ggplot(data, aes(x = utc, y = pm25)) +
      geom_line(color = "blue") +
      labs(
        title = "PM2.5 Trend Over Time",
        x = "Date",
        y = "PM2.5 (µg/m³)"
      ) +
      theme_light()
  )
}

# Correlation Heatmap
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE,
           title = "Correlation Heatmap")


# 9. Interpretation

cat("\nINTERPRETATION:\n")
cat("1. PM2.5 and PM10 are highly correlated — likely from similar emission sources.\n")
cat("2. Pollutant levels vary by location, with some urban sites showing higher readings.\n")
cat("3. Seasonal or temporal trends may indicate higher pollution in colder months.\n")
