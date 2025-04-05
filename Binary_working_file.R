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
    Train_Delayed = case_when(
      Delay_Category == "On Time" ~ FALSE,    
      Delay_Category == "Unknown" ~ NA,
      Delay_Category %in% c("Minor Delay", "Moderate Delay", "Significant Delay") ~ TRUE,  
      TRUE ~ NA  # Handle any other unrecognized categories as NA
    ),
    
   
    Train_RUSH_HOUR = case_when(
      RUSH_HOUR == "rush_hour_none" ~ FALSE,
      RUSH_HOUR %in% c("rush_hour_vormittag", "rush_hour_abend") ~ TRUE,
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


##Reducing the subbset just with the relevant columns
zb_final_binominal <- zb_final_binominal %>%
  select(
    BETRIEBSTAG,
    LINIEN_TEXT,
    FAELLT_AUS_TF,
    HALTESTELLEN_NAME,
    ANKUNFTSZEIT,
    AN_PROGNOSE,
    AN_PROGNOSE_STATUS,
    ABFAHRTSZEIT,
    AB_PROGNOSE,
    AB_PROGNOSE_STATUS,
    ABFAHRTDELAY_min,
    ANKUNFTDELAY_min,
    Delay_Category,
    Train_Delayed,
    TAGESZEIT,
    RUSH_HOUR,
    Train_RUSH_HOUR,
    STUNDE_ANKUNFT
  )

str(zb_final_binominal)

View(zb_final_binominal)


#Models

glm_trial <- glm(Train_Delayed ~ LINIEN_TEXT, family = "binomial", data = zb_final_binominal)

summary(glm_trial)





