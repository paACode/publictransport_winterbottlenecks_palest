#Binary and binominal
# Load libraries
library(dplyr)
library(ggplot2)
library(mgcv)

# Load the data
zb_final <- read.csv("zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

str(zb_final)

# Define relevant train stations
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")

# Full transformation pipeline
zb_final_binominal <- zb_final %>%
  # Create binominal columns
  mutate(
    Train_Delayed = case_when(
      Delay_Category == "On Time" ~ FALSE,    
      Delay_Category == "Unknown" ~ NA,
      Delay_Category %in% c("Minor Delay", "Moderate Delay", "Significant Delay") ~ TRUE,  
      TRUE ~ NA
    ),
    Train_RUSH_HOUR = case_when(
      RUSH_HOUR == "rush_hour_none" ~ FALSE,
      RUSH_HOUR %in% c("rush_hour_vormittag", "rush_hour_abend") ~ TRUE,
      is.na(RUSH_HOUR) ~ NA,
      TRUE ~ NA
    )
  ) %>%
  # Filter for relevant stations
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep) %>%
  # Keep relevant columns
  select(BETRIEBSTAG, LINIEN_TEXT, FAELLT_AUS_TF, HALTESTELLEN_NAME,
         ANKUNFTSZEIT, AN_PROGNOSE, AN_PROGNOSE_STATUS, ABFAHRTSZEIT,
         AB_PROGNOSE, AB_PROGNOSE_STATUS, ABFAHRTDELAY_min, ANKUNFTDELAY_min,
         Delay_Category, Train_Delayed, TAGESZEIT, RUSH_HOUR, Train_RUSH_HOUR,
         STUNDE_ANKUNFT, w_temp_avg_c_Luzern) %>%
  
  # Convert the Train_Delayed to numeric (0 = FALSE, 1 = TRUE) in order to be able to use it for a logistic regression model
  zb_final_binominal$Train_Delayed <- as.numeric(zb_final_binominal$Train_Delayed)

# Remove rows with missing delay data
filter(!is.na(ANKUNFTDELAY_min), !is.na(ABFAHRTDELAY_min))

# Check for any remaining NAs
sum(is.na(zb_final_binominal$ANKUNFTDELAY_min))
sum(is.na(zb_final_binominal$ABFAHRTDELAY_min))

# Inspect structure and view
str(zb_final_binominal)s
View(zb_final_binominal)


glm_trial <- glm(Train_Delayed ~ w_temp_avg_c_Luzern, family = "binomial", data = zb_final_binominal)

summary(glm_trial)


ggplot(zb_final_binominal, aes(x = w_temp_avg_c_Luzern, y = as.numeric(Train_Delayed))) +
  geom_point(alpha = 0.1) +  # Add jitter or transparency if needed
  geom_smooth(method = "gam", formula = y ~ s(x), method.args = list(family = "binomial")) +
  labs(title = "Probability of Train Delay vs Temperature",
       x = "Average Temperature in Luzern (°C)",
       y = "Probability of Train Being Delayed") +
  theme_minimal()


