# Generalised Additive Models GAM

#General codes
library(dplyr)
library(ggplot2)
library(mgcv)

"In this project, we apply Generalized Additive Models (GAMs) to analyze the effects of various predictors on train delays, specifically focusing on the response variables ANKUNFTDELAY_min (arrival delay in minutes) and ABFAHRTDELAY_min (departure delay in minutes). 
Due to the limited availability of continuous variables in our dataset these two delay metrics will be the response variables we will focus on.
A key subset of our predictors is related to weather conditions. However, because the ZB Bahn railway line spans multiple climatic regions, 
comparing weather-related predictors across all stations introduces variability that may obscure meaningful patterns. To address this, we restrict our analysis to a subset of stations located within the Lucerne region, where the climate is more uniform. 
This allows for a more reliable interpretation of the relationship between weather conditions and train delays.
"

#Creating subset

zb_final <- read.csv("zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

View(zb_final)

str(zb_final)

## Subset the data

# Define the specific Haltestellen
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")

# Subset the data
zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)

zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)

str(zb_final_subset)

#Removing rows NA in ANKUNFTDELAY_min and ABFAHRTDELAY_min

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_subset$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_subset$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0

#Removing trains which have arrived or departured earlier
zb_final_subset <- zb_final_subset %>%
  filter(ANKUNFTDELAY_min >= 0, ABFAHRTDELAY_min >= 0)

#Reducing the subbset just with the relevant columns

zb_final_subset <- zb_final_subset %>% select(BETRIEBSTAG, LINIEN_TEXT, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, ABFAHRTSZEIT, AB_PROGNOSE, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, TAGESZEIT, RUSH_HOUR, w_precip_mm_Luzern, w_temp_max_c_Luzern, STUNDE_ANKUNFT)


View(zb_final_subset)



"R assumes for GAM models usually the gaussian family for the response variables.Therefore, lets have a look if the response variables ANKUNFTDELAY_min and ABFAHRTDELAY_min are gaussian distributed. 
"

par(mfrow = c(1, 2))


hist(zb_final$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightblue", breaks = 10, xlim = c(-2,8))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightblue", xlim = c(-2,8))

"We see that both response variables are not normally distrubuted. They seem to be right skewed. Lets check with the QQ plots. 
"


qqnorm(zb_final$ANKUNFTDELAY_min, main = "QQ Plot of ANKUNFTDELAY_min")
qqline(zb_final$ANKUNFTDELAY_min, col = "red")

qqnorm(zb_final_subset$ABFAHRTDELAY_min, main = "QQ Plot of ABFAHRTDELAY_min")
qqline(zb_final_subset$ABFAHRTDELAY_min, col = "red")

"The QQ plots indicate that the response variables are right-skewed. Therefore, we apply a log transformation to improve the distribution and better meet model assumptions."

# Log transformation (handling 0 values with +1)
zb_final_subset$log_ANKUNFTDELAY <- log(zb_final_subset$ANKUNFTDELAY_min + 1)
zb_final_subset$log_ABFAHRTDELAY <- log(zb_final_subset$ABFAHRTDELAY_min + 1)


# QQ Plot for log(ANKUNFTDELAY_min + 1)
qqnorm(zb_final_subset$log_ANKUNFTDELAY,
       main = "QQ Plot of log(ANKUNFTDELAY_min + 1)",
       col = "darkblue", pch = 19)
qqline(zb_final_subset$log_ANKUNFTDELAY, col = "red", lwd = 2)

# QQ Plot for log(ABFAHRTDELAY_min + 1)
qqnorm(zb_final_subset$log_ABFAHRTDELAY,
       main = "QQ Plot of log(ABFAHRTDELAY_min + 1)",
       col = "darkblue", pch = 19)
qqline(zb_final_subset$log_ABFAHRTDELAY, col = "red", lwd = 2)

