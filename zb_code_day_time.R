zentralbahn_delay <- read.csv("zentrahlbahn_nov_dataset_delay.csv")

str(zentralbahn_delay)


# Load necessary library
library(dplyr)

# Read the dataset
zentralbahn_delay <- read.csv("zentrahlbahn_nov_dataset_delay.csv")

# Check structure of dataset before processing
str(zentralbahn_delay)

# Create a copy of the dataset and extract STUNDE_ANKUNFT
zentralbahn_TAGESZEIT <- zentralbahn_delay %>%
  mutate(
    STUNDE_ANKUNFT = as.numeric(format(as.POSIXct(ANKUNFTSZEIT, format="%Y-%m-%d %H:%M:%S"), "%H"))
  )

# Debug: Print unique values of STUNDE_ANKUNFT to check if conversion worked
print(unique(zentralbahn_TAGESZEIT$STUNDE_ANKUNFT))

# Classify the time of day
zentralbahn_TAGESZEIT <- zentralbahn_TAGESZEIT %>%
  mutate(
    TAGESZEIT = case_when(
      STUNDE_ANKUNFT >= 6  & STUNDE_ANKUNFT <= 10 ~ "Vormittag",
      STUNDE_ANKUNFT >= 11 & STUNDE_ANKUNFT <= 13 ~ "Mittag",
      STUNDE_ANKUNFT >= 14 & STUNDE_ANKUNFT <= 16 ~ "Nachmittag",
      STUNDE_ANKUNFT >= 17 & STUNDE_ANKUNFT <= 19 ~ "Abend",
      (STUNDE_ANKUNFT >= 20 & STUNDE_ANKUNFT <= 23) | (STUNDE_ANKUNFT >= 0 & STUNDE_ANKUNFT <= 5) ~ "Nacht",
      TRUE ~ NA_character_  # Handles missing values
    )
  )

# Debug: Check if STUNDE_ANKUNFT values match the expected range
print(table(zentralbahn_TAGESZEIT$STUNDE_ANKUNFT, useNA = "always"))

# Add Pendler_Rush_Hour classification
zentralbahn_TAGESZEIT <- zentralbahn_TAGESZEIT %>%
  mutate(
    Pendler_Rush_Hour = case_when(
      is.na(STUNDE_ANKUNFT) ~ NA_character_,  # If no arrival hour, keep NA
      STUNDE_ANKUNFT >= 6 & STUNDE_ANKUNFT <= 8  ~ "vormittag.pendler.rush.hour",
      STUNDE_ANKUNFT >= 17 & STUNDE_ANKUNFT <= 18 ~ "abend.pendler.rush.hour",
      TRUE ~ "keine.pendler.rush.hour"  # Everything else gets "keine.pendler.rush.hour"
    )
  )

# Debug: Check if Pendler_Rush_Hour is populated correctly
print(table(zentralbahn_TAGESZEIT$Pendler_Rush_Hour, useNA = "always"))

# View the dataset
View(zentralbahn_TAGESZEIT)

# Save to CSV
write.csv(zentralbahn_TAGESZEIT, "zentralbahn_day_time.csv", row.names = FALSE)