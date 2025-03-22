zentralbahn_delay <- read.csv("zentrahlbahn_nov_dataset_delay.csv")

str(zentralbahn_delay)


# Load necessary library
library(dplyr)

# Read the dataset
zentralbahn_delay <- read.csv("zentrahlbahn_nov_dataset_delay.csv")

# Check structure of dataset before processing
str(zentralbahn_delay)

# Create a copy of the dataset and extract Hour_Ankunft
zentralbahn_day_time <- zentralbahn_delay %>%
  mutate(
    Hour_Ankunft = as.numeric(format(as.POSIXct(ANKUNFTSZEIT, format="%Y-%m-%d %H:%M:%S"), "%H"))
  )

# Debug: Print unique values of Hour_Ankunft to check if conversion worked
print(unique(zentralbahn_day_time$Hour_Ankunft))

# Classify the time of day
zentralbahn_day_time <- zentralbahn_day_time %>%
  mutate(
    day_time = case_when(
      Hour_Ankunft >= 6  & Hour_Ankunft <= 10 ~ "Vormittag",
      Hour_Ankunft >= 11 & Hour_Ankunft <= 13 ~ "Mittag",
      Hour_Ankunft >= 14 & Hour_Ankunft <= 16 ~ "Nachmittag",
      Hour_Ankunft >= 17 & Hour_Ankunft <= 19 ~ "Abend",
      (Hour_Ankunft >= 20 & Hour_Ankunft <= 23) | (Hour_Ankunft >= 0 & Hour_Ankunft <= 5) ~ "Nacht",
      TRUE ~ NA_character_  # Handles missing values
    )
  )

# Debug: Check if Hour_Ankunft values match the expected range
print(table(zentralbahn_day_time$Hour_Ankunft, useNA = "always"))

# Add Pendler_Rush_Hour classification
zentralbahn_day_time <- zentralbahn_day_time %>%
  mutate(
    Pendler_Rush_Hour = case_when(
      is.na(Hour_Ankunft) ~ NA_character_,  # If no arrival hour, keep NA
      Hour_Ankunft >= 6 & Hour_Ankunft <= 8  ~ "vormittag.pendler.rush.hour",
      Hour_Ankunft >= 17 & Hour_Ankunft <= 18 ~ "abend.pendler.rush.hour",
      TRUE ~ "keine.pendler.rush.hour"  # Everything else gets "keine.pendler.rush.hour"
    )
  )

# Debug: Check if Pendler_Rush_Hour is populated correctly
print(table(zentralbahn_day_time$Pendler_Rush_Hour, useNA = "always"))

# View the dataset
View(zentralbahn_day_time)

# Save to CSV
write.csv(zentralbahn_day_time, "zentralbahn_day_time.csv", row.names = FALSE)