# Generalised Additive Models GAM

#General codes
library(dplyr)
library(ggplot2)
library(mgcv)
library(tidyr)

"In this project, we apply Generalized Additive Models (GAMs) to analyze the effects of various predictors on train delays, specifically focusing on the response variables ANKUNFTDELAY_min (arrival delay in minutes) and ABFAHRTDELAY_min (departure delay in minutes). 
Due to the limited availability of continuous variables in our dataset these two delay metrics will be the response variables we will focus on.
A key subset of our predictors is related to weather conditions. However, because the ZB Bahn railway line spans multiple climatic regions, 
comparing weather-related predictors across all stations introduces variability that may obscure meaningful patterns. To address this, we restrict our analysis to a subset of stations located within the Lucerne region, where the climate is more uniform. 
This allows for a more reliable interpretation of the relationship between weather conditions and train delays.
"

#Creating subset

zb_final <- read.csv("zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

## Subset the data

# Define the specific Haltestellen
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")

# Subset the data

zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)


#Removing rows NA in ANKUNFTDELAY_min and ABFAHRTDELAY_min

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_subset$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_subset$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0

#Reducing the subbset just with the relevant columns

zb_final_subset <- zb_final_subset %>% select(BETRIEBSTAG, LINIEN_TEXT, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, ABFAHRTSZEIT, AB_PROGNOSE, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, TAGESZEIT, RUSH_HOUR, w_precip_mm_Luzern, w_temp_max_c_Luzern, STUNDE_ANKUNFT)


str(zb_final_subset)

View(zb_final_subset)



"R assumes for GAM models usually the gaussian family for the response variables.Therefore, lets have a look if the response variables ANKUNFTDELAY_min and ABFAHRTDELAY_min are gaussian distributed. 
"

par(mfrow = c(1, 2))


hist(zb_final_subset$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightblue", breaks = 100, xlim = c(-2,8))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightblue", breaks = 100, xlim = c(-2,8))

"We see that both response variables are not normally distrubuted. They seem to be right skewed. Lets check with the QQ plots. 
"


qqnorm(zb_final$ANKUNFTDELAY_min, main = "QQ Plot of ANKUNFTDELAY_min")
qqline(zb_final$ANKUNFTDELAY_min, col = "lightblue")

qqnorm(zb_final_subset$ABFAHRTDELAY_min, main = "QQ Plot of ABFAHRTDELAY_min")
qqline(zb_final_subset$ABFAHRTDELAY_min, col = "lightblue")

"The QQ plots reveal that the response variables are right-skewed, which aligns with expectations. 
In real-world train operations, early arrivals or departures are relatively uncommon compared to delays. 
Moreover, the distribution indicates that longer delays occur less frequently than shorter ones, reflecting 
typical delay patterns observed in Swiss public transportation. For this reason, we restrict our dataset for the 
GAM analysis to observations with delays between -2 and +4 minutes, aiming to ensure a distribution that more closely 
approximates the Gaussian assumption required by the model.
"

zb_final_subset <- zb_final_subset %>%
  filter(ABFAHRTDELAY_min >= -2 & ABFAHRTDELAY_min <= 4,
         ANKUNFTDELAY_min >= -2 & ANKUNFTDELAY_min <= 4)


hist(zb_final_subset$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightgreen", breaks = 50, xlim = c(-2,4))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightgreen", breaks = 50, xlim = c(-2,4))


