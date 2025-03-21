zentralbahn_delay <- read.csv("zentrahlbahn_nov_dataset_delay.csv")

str(zentralbahn_delay)

# Load necessary library
library(dplyr)

# Create a copy of the dataset
zentralbahn_day_time <- zentralbahn_delay %>%
  
  #create column Hour_Ankunfts
  
  mutate(Hour_Ankunft = format(as.POSIXct(ANKUNFTSZEIT, format="%Y-%m-%d %H:%M:%S"), "%H"))


#Classify the Hour_Ankunfts
zentralbahn_day_time <- zentralbahn_day_time %>%
  mutate(
    Hour_Ankunft = as.numeric(Hour_Ankunft),  # Convert Hour_Ankunft to numeric (if it's not already)
    day_time = case_when(
      Hour_Ankunft >= 6  & Hour_Ankunft <= 10 ~ "Vormittag",
      Hour_Ankunft >= 11 & Hour_Ankunft <= 13 ~ "Mittag",
      Hour_Ankunft >= 14 & Hour_Ankunft <= 16 ~ "Nachmittag",
      Hour_Ankunft >= 17 & Hour_Ankunft <= 19 ~ "Abend",
      (Hour_Ankunft >= 20 & Hour_Ankunft <= 23) | (Hour_Ankunft >= 0 & Hour_Ankunft <= 5) ~ "Nacht",
      TRUE ~ NA_character_  # Handles missing or unexpected values
    )
  )
View (zentralbahn_day_time)

write.csv(zentralbahn_day_time, "zentralbahn_day_time.csv", row.names = FALSE)

