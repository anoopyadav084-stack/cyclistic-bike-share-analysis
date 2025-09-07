# CYCLISTIC BIKE-SHARE ANALYSIS
# Google Data Analytics Capstone Project

# 1.0 CHECK AND INSTALL PACKAGES ----
required_packages <- c("tidyverse", "lubridate", "skimr", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# 2.0 LOAD LIBRARIES ----
library(tidyverse)
library(lubridate)
library(skimr)
library(here)

# 3.0 SET WORKING DIRECTORY ----
setwd("C:/Users/anoop/Downloads/cyclistic-project")
print(paste("Current working directory:", getwd()))

# 4.0 FIND AND VERIFY CSV FILES ----
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in: C:/Users/anoop/Downloads/cyclistic-project")
}

print("CSV files found:")
print(csv_files)

# 5.0 READ FIRST FILE TO CHECK COLUMN NAMES ----
first_file <- read_csv(csv_files[1], n_max = 5) # Read only first 5 rows for speed
print("Original column names in first file:")
print(colnames(first_file))

# 6.0 READ AND COMBINE ALL FILES WITH AUTOMATIC RENAMING ----
combined_data <- map_dfr(csv_files, ~{
  data <- read_csv(.x)
  
  # Convert column names to standard format
  colnames(data) <- colnames(data) %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    gsub("-", "_", .)
  
  return(data)
})

# 7.0 VERIFY FINAL COLUMN NAMES ----
print("Final standardized column names:")
print(colnames(combined_data))

# 8.0 CLEAN AND PROCESS DATA ----
cyclistic_clean <- combined_data %>%
  # Convert to proper datetime format
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    # Create new analysis columns
    ride_date = as.Date(started_at),
    ride_month = month(started_at, label = TRUE),
    day_of_week = wday(started_at, label = TRUE, week_start = 1),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  ) %>%
  # Remove invalid data
  filter(
    ride_length > 1,          # Remove rides shorter than 1 minute
    ride_length < 1440,       # Remove rides longer than 24 hours
    !is.na(start_lat),        # Remove rides without location data
    !is.na(start_lng)
  )

# 9.0 BASIC ANALYSIS ----
print("Dataset overview:")
glimpse(cyclistic_clean)

print("Ride length summary (minutes):")
summary(cyclistic_clean$ride_length)

print("Number of rides by user type:")
cyclistic_clean %>%
  count(member_casual, name = "total_rides") %>%
  print()

print("Average ride length by user type (minutes):")
cyclistic_clean %>%
  group_by(member_casual) %>%
  summarise(average_duration = mean(ride_length)) %>%
  print()

# 10.0 SAVE CLEANED DATA FOR TABLEAU ----
write_csv(cyclistic_clean, "cyclistic_data_clean.csv")
print("Analysis complete!")
print(paste("Cleaned data saved as:", file.path(getwd(), "cyclistic_data_clean.csv")))