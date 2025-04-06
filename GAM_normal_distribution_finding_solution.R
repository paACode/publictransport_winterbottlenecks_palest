# Generalised Additive Models GAM

#General codes
library(dplyr)
library(ggplot2)
library(mgcv)

zb_final <- read.csv("zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

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

#Removing rows NA in ANKUNFTDELAY_min

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_subset$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_subset$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0

#Keeping just the columns which I need for these models

zb_final_subset <- zb_final_subset %>% select(BETRIEBSTAG, LINIEN_TEXT, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, ABFAHRTSZEIT, AB_PROGNOSE, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, TAGESZEIT, RUSH_HOUR, w_pressure_hpa_Luzern, w_temp_max_c_Luzern, STUNDE_ANKUNFT)

View(zb_final_subset)

"For the GAM we will analyse the effect of different predictors on the response variables ANKUNFTDELAY_min and ABFAHRTDELAY_min. As our dataset does not contain a wide range of continous data, which is necessary for a GAM model,
ANKUNFTDELAY_min and ABFAHRTDELAY_min are the only response variable we will use. 


R assumes for GAM models usually the gaussian family for the predictors. In the upcoming GAM models we will mostly use w_temp_avg_c_Luzern and w_precip_mm_Luzern as predictors.
Therefore, lets have a look if they are gaussian distributed. 
"

par(mfrow = c(1, 2))


hist(zb_final_subset$w_temp_avg_c_Luzern, main = "Histogram of temp_avg_c_Luzern", xlab = "Temperature (C)", col = "lightblue", breaks = 30)

hist(zb_final_subset$w_precip_mm_Luzern, main = "Histogram of $w_precip_mm_Luzern", xlab = "Precipitation (mm)", col = "lightblue", breaks = 30)

"We see that both predictors are not normally distributed. Lets have a look at the QQ Plot
"


qqnorm(zb_final_subset$w_temp_avg_c_Luzern, main = "QQ Plot of temp_avg_c_Luzern")
qqline(zb_final_subset$w_temp_avg_c_Luzern, col = "red")

qqnorm(zb_final_subset$w_precip_mm_Luzern, main = "QQ Plot of w_precip_mm_Luzern")
qqline(zb_final_subset$w_precip_mm_Luzern, col = "red")

"Also in the WW Plot we see that the predictors are not normally distributed."

"Therefore, we have to change the familly for modelling the GAM. 
There are two options: Gamma distribution or Tweedie distribution.

The challenge is that those models can not handle negative values in the response variables. Delays can not only be delayed but they can also be too early. For example if trains arrive earlier than their planned arrvial time. 

The challenges not normal distribution on the predictor variable side and negative variable on the response predictor side create a conflict of aim. We have draw some limitation in order to be able to fit a GAM model.
The solution is that we will focus in GAM models only on delayed arrival / depature and neglegt the early arrival / depatures. The GAM requirements will still be fulfilled as delays do not have integer numbers.

Therefore, we create a subset:
"

zb_final_subset <- zb_final_subset %>%
  filter(ANKUNFTDELAY_min >= 0 & ABFAHRTDELAY_min >= 0)

View(zb_final_subset)


# Plot histogram of the response variable
hist(zb_final_subset$ANKUNFTDELAY_min, main="Histogram of ANKUNFTDELAY_min", xlab="Arrival Delay (minutes)", col="lightblue", breaks=50)
hist(zb_final_subset$ABFAHRTDELAY_min, main="Histogram of ANKUNFTDELAY_min", xlab="Arrival Delay (minutes)", col="lightblue", breaks=50)



"Here we see that ANKUNFTDELAY_min and ABFAHRTDELAY_min have a large amount of 0. This indiactes that we should rather use Tweedie Distribution.
"

# Check proportion of zeros in the response variable
sum(zb_final_subset$ANKUNFTDELAY_min == 0) / length(zb_final_subset$ANKUNFTDELAY_min)
