"KANN NACH DER BESPRECHUNG MIT PASCAL AM 19.05.25 WIEDER GELÖSCHT WERDEN. NUR AUFBEWAHRT, FALLS WIR ETWAS AUSPROBIEREN MÖCHTEN."


In this project, we apply Generalized Additive Models (GAMs) to analyze the effects of various predictors on train delays, specifically focusing on the response variables ANKUNFTDELAY_min (arrival delay in minutes) and ABFAHRTDELAY_min (departure delay in minutes). 
Due to the limited availability of continuous variables in our dataset these two delay metrics will be the response variables we will focus on.
A key subset of our predictors is related to weather conditions. However, because the ZB Bahn railway line spans multiple climatic regions, 
comparing weather-related predictors across all stations introduces variability that may obscure meaningful patterns. To address this, we restrict our analysis to a subset of stations located within the Lucerne region, where the climate is more uniform. 
This allows for a more reliable interpretation of the relationship between weather conditions and train delays.

{r gam-general-codes-subset, include=TRUE, echo=TRUE}

#Creating subset

zb_final <- read.csv("data/zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

# Define the specific Haltestellen in the region of Lucerne
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")

zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)

# Create new columns with delay in minutes
zb_final_subset <- zb_final_subset %>%
  mutate(
    ABFAHRTDELAY_min = ABFAHRTDELAY_sec / 60,
    ANKUNFTDELAY_min = ANKUNFTDELAY_sec / 60
  )


#Removing rows NA in ANKUNFTDELAY_min and ABFAHRTDELAY_min

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_subset$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_subset$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0

#Reducing the subbset just with the relevant columns

zb_final_subset <- zb_final_subset %>% select(BETRIEBSTAG, LINIEN_TEXT, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, ABFAHRTSZEIT, AB_PROGNOSE, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, TAGESZEIT, RUSH_HOUR, w_precip_mm_Luzern, w_temp_avg_c_Luzern)



R assumes for GAM models usually the gaussian family for the response variables. Therefore, let's have a look if the response variables ANKUNFTDELAY_min and ABFAHRTDELAY_min are gaussian distributed. 

{r first-histogram, include=TRUE, echo=TRUE}

par(mfrow = c(1, 2))


hist(zb_final_subset$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightblue", breaks = 100, xlim = c(-2,8))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightblue", breaks = 100, xlim = c(-2,8))




We see that both response variables are not normally distributed They seem to be right skewed. Lets check with the QQ plots. 

{r first-qqplot, include=TRUE, echo=TRUE}

par(mfrow = c(1, 2))

qqnorm(zb_final$ANKUNFTDELAY_min, main = "QQ Plot of ANKUNFTDELAY_min")
qqline(zb_final$ANKUNFTDELAY_min, col = "lightblue")

qqnorm(zb_final_subset$ABFAHRTDELAY_min, main = "QQ Plot of ABFAHRTDELAY_min")
qqline(zb_final_subset$ABFAHRTDELAY_min, col = "lightblue")



The QQ plots reveal that the response variables are right-skewed, which aligns with expectations. In real-world train operations, early arrivals or departures are relatively uncommon compared to delays. Moreover, the distribution indicates that longer delays occur less frequently than shorter ones, reflecting 
typical delay patterns observed in Swiss public transportation. For this reason, we restrict our dataset for the 
GAM analysis to observations with delays between -2 and +4 minutes, aiming to ensure a distribution that more closely 
approximates the Gaussian assumption required by the model

{r second-histogram, include=TRUE, echo=TRUE}

par(mfrow = c(1, 2))

zb_final_subset <- zb_final_subset %>%
  filter(ABFAHRTDELAY_min >= -2 & ABFAHRTDELAY_min <= 4,
         ANKUNFTDELAY_min >= -2 & ANKUNFTDELAY_min <= 4)


hist(zb_final_subset$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightgreen", breaks = 50, xlim = c(-2,4))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightgreen", breaks = 50, xlim = c(-2,4))




The histogram indicates an approximately normal distribution. Therefore, the dataset is now suitable for fitting with Generalized Additive Models (GAMs).

{r gam-Ankunft-temp, include=TRUE, echo=TRUE}

gam_Ankunft_temp <- gam(ANKUNFTDELAY_min ~ s(w_temp_avg_c_Luzern), data = zb_final_subset) 

summary (gam_Ankunft_temp)


The p-value associated with the smooth term is close to zero, providing strong evidence that the average temperature in Lucerne has a statistically significant effect on arrival delays. The estimated effective degrees of freedom (edf ≈ 9) suggest a moderately complex, non-linear relationship. However, the adjusted R-squared value of 0.036 and the explained deviance of only 3.67% indicate that the model captures only a small portion of the variation in arrival delays. Thus, while the effect is significant, temperature alone does not explain much of the delay variability.
###############
{r gam-temp-precip-tageszeit-linien_text, include=TRUE, echo=TRUE}

gam_temp_precip_tageszeit_linien_text <- gam(ABFAHRTDELAY_min ~ TAGESZEIT + LINIEN_TEXT + s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern), data = zb_final_subset) 

summary(gam_temp_precip_tageszeit_linien_text)

plot(gam_temp_precip_tageszeit_linien_text, residuals = TRUE, select = 1,main = "Effect of Temperature on Departure Delay")
plot(gam_temp_precip_tageszeit_linien_text, residuals = TRUE, select = 2, main = "Effect of Precipitation on Departure Delay")
#################



library(dplyr)
library(ggplot2)
library(patchwork)

# Step 1: Create bins for temperature and precipitation
zb_final_subset <- zb_final_subset %>%
  mutate(
    temp_bin = cut(w_temp_avg_c_Luzern, breaks = seq(min(w_temp_avg_c_Luzern), max(w_temp_avg_c_Luzern), by = 0.5), include.lowest = TRUE),
    precip_bin = cut(w_precip_mm_Luzern, breaks = seq(min(w_precip_mm_Luzern), max(w_precip_mm_Luzern), by = 1), include.lowest = TRUE)
  )

# Step 2: Aggregate average departure delay by bins
temp_delay_aggregated <- zb_final_subset %>%
  group_by(temp_bin) %>%
  summarise(
    avg_ABFAHRTDELAY_min = mean(ABFAHRTDELAY_min, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n > 5) 

precip_delay_aggregated <- zb_final_subset %>%
  group_by(precip_bin) %>%
  summarise(
    avg_ABFAHRTDELAY_min = mean(ABFAHRTDELAY_min, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n > 5)

# Step 3: Extract the lower bound of each bin for plotting
temp_delay_aggregated <- temp_delay_aggregated %>%
  mutate(temp_bin_num = as.numeric(sub("\\((.+),.*", "\\1", temp_bin)))

precip_delay_aggregated <- precip_delay_aggregated %>%
  mutate(precip_bin_num = as.numeric(sub("\\((.+),.*", "\\1", precip_bin)))

# Step 4: Plot the aggregated average delays
plot_temp <- ggplot(temp_delay_aggregated, aes(x = temp_bin_num, y = avg_ABFAHRTDELAY_min)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
  labs(
    title = "Average Departure Delay vs Temperature (Aggregated)",
    x = "Temperature (°C)",
    y = "Average Departure Delay (min)"
  ) +
  theme_minimal()

plot_precip <- ggplot(precip_delay_aggregated, aes(x = precip_bin_num, y = avg_ABFAHRTDELAY_min)) +
  geom_point(color = "green", size = 2) +
  geom_smooth(method = "loess", color = "darkgreen", se = TRUE) +
  labs(
    title = "Average Departure Delay vs Precipitation (Aggregated)",
    x = "Precipitation (mm)",
    y = "Average Departure Delay (min)"
  ) +
  theme_minimal()

# Display both plots side by side
plot_temp + plot_precip
