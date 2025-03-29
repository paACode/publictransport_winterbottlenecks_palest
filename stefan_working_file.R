#General codes
zb_final <- read.csv("zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

str(zb_final)

#First overview with plot

plot(ANKUNFTDELAY_sec ~ TAGESZEIT, data = zb_final)
"Similar spread, a lot of outlierts"

plot(ANKUNFTDELAY_sec ~ RUSH_HOUR, data = zb_final)
"Similar spread but narrower, a lot of outlierts"

# Define colors for each category in TAGESZEIT
color_map <- c("Abend" = "wheat4", 
               "Vormittag" = "slateblue4", 
               "Nachmittag" = "lightpink1", 
               "Mittag" = "lightgreen", 
               "Nacht" = "orangered2")


"We see that there many delays in the morning at around 7.8C. In addition, it seems that there as well many delays in the night over all temperatures but the longest delays are at 0 and 6C."
# Assign colors based on TAGESZEIT
colors <- color_map[as.character(zb_final$TAGESZEIT)]

# Plot with assigned colors
plot(zb_final$w_temp_avg_c_Luzern, zb_final$ABFAHRTDELAY_sec, 
     col = colors, pch = 19,
     xlab = "Average Temperature (Luzern)", 
     ylab = "Departure Delay (sec)")

# Add a legend
legend("topleft", pch = 19, legend = names(color_map), col = color_map)


library(ggplot2)

qplot(y = w_temp_avg_c_Luzern, 
      x = ANKUNFTDELAY_min, 
      facets = ~ TAGESZEIT, 
      data = zb_final)

"There sems to be a cluster right below 2.5C and more obvious above 7.5C over all the days. In the morning the longest delays are at roughly 7.7C. The shortest delays are in the evening."


#Simple regression model

# Convert ANKUNFTSZEIT from factor to character
zb_final$ANKUNFTSZEIT_CHAR <- as.character(zb_final$ANKUNFTSZEIT)

# Extract the hour from the timestamp
zb_final$ANKUNFTSZEIT_HOUR <- as.numeric(substr(zb_final$ANKUNFTSZEIT_CHAR, 12, 13))

# Fit the linear model using only the hour of arrival
lm.temperature <- lm(ANKUNFTDELAY_sec ~ ANKUNFTSZEIT_HOUR, data = zb_final)

# View the summary
summary(lm.temperature)

"At midnight (00:00), the expected arrival delay is 42.92 seconds. For each additional hour later in the day, the delay decreases by 0.30 seconds on average."
"The p-value is statistical significant and below so 0.05. Therefore we can be sure certain that it is not 0. So it have statistical significant effect."
"However, the R^2 is very low, showing that the model doesnt represent well the reality. "

#Simple regression model with additional predictor

lm.temperature_updated <- lm(ANKUNFTDELAY_sec ~ ANKUNFTSZEIT_HOUR + w_temp_avg_c_Luzern , data = zb_final)

summary(lm.temperature_updated)

"At midnight (00:00) and 0°C, the expected delay is 47.91 sec. For each additional hour later in the day, 
the delay decreases by 0.30 sec on average. Additionally, for each 1°C increase in temperature, the delay decreases by 0.95 sec. 
Both predictors are statistically significant (p < 0.05), but the model explains less than 0.1% of the variance, meaning other factors 
have a much stronger impact on delay."


lm.temperature_taageszeit <- lm(ANKUNFTDELAY_sec ~ TAGESZEIT + w_temp_avg_c_Luzern , data = zb_final)

summary(lm.temperature_taageszeit)

"The delay in the evening with a temperature of 0C is 57.96 sec.
At lunch time the delay is 27.3 sec less than in the evening.
In the afternoon the delay is 19.29 less than in the evening
In the night the delay is 22.28 sec less than in the evening
In the morning the delay is 3.98 sec les than in the evening
With every unit increas in temperature, the delay reduces by 0.975 sec.
All predictors are statistically siginificant. However the R^2 tell us with 0.012 that other factors have a much stronger impact on delay"


# Regression model with interaction

lm.temperature_tageszeit_interaction <- lm(ANKUNFTDELAY_sec ~ TAGESZEIT * w_temp_avg_c_Luzern , data = zb_final)

summary(lm.temperature_tageszeit_interaction)



#--------------------------------------------------
#GAM


#creating a subset
"I would to analyse the data in function of delay with weather data among others. The ZB line goes throw different climate region. There I want to analyse just the ZB line which is in the region of Lucerne. "

library(dplyr)

# Define the specific Haltestellen
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")

# Subset the data
zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)

summary(zb_final_subset$HALTESTELLEN_NAME)

View(zb_final_subset)

str(zb_final_subset)

#Some plots


library(ggplot2)

gg.delay_hour <- ggplot (data=zb_final_subset, mapping = aes(y= log(ANKUNFTDELAY_min), x=STUNDE_ANKUNFT)) + geom_point()

gg.delay_hour + geom_smooth()

gg.delay_temp <- ggplot (data=zb_final_subset, mapping = aes(y= log(ANKUNFTDELAY_min), x=w_temp_avg_c_Luzern )) + geom_point()

gg.delay_temp + geom_smooth()

gg.delay_precip <- ggplot (data=zb_final_subset, mapping = aes(y= log(ANKUNFTDELAY_min), x=w_precip_mm_Luzern )) + geom_point()

gg.delay_precip + geom_smooth()

"Without log the regression line was almost flat. With log it became more curved. This suggests an increase exponentially rather than linearly with precipitation."


#Model with several parameter measuring collinearity
library(car)
lm.delay_hour_several_parameter_1 <- lm(ANKUNFTDELAY_min ~ STUNDE_ANKUNFT + w_temp_avg_c_Luzern + w_precip_mm_Luzern + RUSH_HOUR + TAGESZEIT, data = zb_final_subset)

summary(lm.delay_hour_several_parameter_1)

vif(lm.delay_hour_several_parameter_1)

"
I will not go into details about the summeray here. The purpose was to see if STUNDE_ANKUNFT is collinear with TAGESZEIT and RUSHOUR, which would make sense because the base of TAGESZEIT and RUSHOUR is STUNDE_ANKUNFT 
In the summary we can see the only predictors which is not statistically significant is STUNDe_ANKUNFT. All other predictos are statistically siginificant. 
VIF shows us that RUSH_HOUR has a GVIF of 6.18 and TAGESZEIT 7.26. This confirms that I should not use those predictors and remove them from my model.
"

#model with several parameters
lm.delay_hour_temp_precip <- lm(ANKUNFTDELAY_min ~ STUNDE_ANKUNFT + w_temp_avg_c_Luzern + w_precip_mm_Luzern, data = zb_final_subset)
 
lm.delay_hour_temp_precip_quadratic <- update(lm.delay_hour_temp_precip, .~. + I(w_precip_mm_Luzern^2)+ I(w_temp_avg_c_Luzern^2)+ I(STUNDE_ANKUNFT^2))
"I have added here 3 quadratic terms. The model fitts better however it is more complex, danger of overfitting. Maybe a model with just one quadratic term should be considered"

anova(lm.delay_hour_temp_precip, lm.delay_hour_temp_precip_quadratic)

"Stopped at page 5 but need to read collinearity isues at page 18 "



