#Binary and binominal

#General codes
library(dplyr)
library(ggplot2)
library(mgcv)


zb_final <- read.csv("zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

str (zb_final)

#Subset
zb_final_binominal <- zb_final %>%
  mutate(
    # Adjust Train_Delayed logic to treat "Unknown" as NA
    Train_Delayed = case_when(
      Delay_Category == "On Time" ~ FALSE,    
      Delay_Category == "Unknown" ~ NA,  # Explicitly set "Unknown" as NA
      Delay_Category %in% c("Minor Delay", "Moderate Delay", "Significant Delay") ~ TRUE,  
      TRUE ~ NA  # Handle any other unrecognized categories as NA
    ),
    
    # Adjust Train_RUSH_HOUR to keep current logic
    Train_RUSH_HOUR = case_when(
      RUSH_HOUR == "rush_hour_none" ~ FALSE,  # "rush_hour_none" is FALSE
      RUSH_HOUR %in% c("rush_hour_vormittag", "rush_hour_abend") ~ TRUE,  # Rush hours are TRUE
      is.na(RUSH_HOUR) ~ NA,  # Handle NA values properly
      TRUE ~ NA  # Any other unrecognized or unexpected RUSH_HOUR values are NA
    )
  ) %>%
  select(
    1:(match("Delay_Category", names(.))),  # Columns up to Delay_Category
    Train_Delayed,  # Add Train_Delayed after Delay_Category
    (match("Delay_Category", names(.)) + 1):(match("RUSH_HOUR", names(.))),  # Columns between Delay_Category and RUSH_HOUR
    Train_RUSH_HOUR,  # Add Train_RUSH_HOUR after RUSH_HOUR
    (match("RUSH_HOUR", names(.)) + 1):ncol(.)  # Columns after RUSH_HOUR
  )



str(zb_final_binominal)

View(zb_final_binominal)