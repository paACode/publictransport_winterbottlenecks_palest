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
# Generalised Additive Models GAM

##Polynominals (not sure if include in the final file)
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


#Model with several parameter measuring collinearity (not sure if include in the final file)
library(car)
lm.delay_hour_several_parameter_1 <- lm(ANKUNFTDELAY_min ~ STUNDE_ANKUNFT + w_temp_avg_c_Luzern + w_precip_mm_Luzern + RUSH_HOUR + TAGESZEIT, data = zb_final_subset)

summary(lm.delay_hour_several_parameter_1)

vif(lm.delay_hour_several_parameter_1)

"
I will not go into details about the summeray here. The purpose was to see if STUNDE_ANKUNFT is collinear with TAGESZEIT and RUSHOUR, which would make sense because the base of TAGESZEIT and RUSHOUR is STUNDE_ANKUNFT 
In the summary we can see the only predictors which is not statistically significant is STUNDe_ANKUNFT. All other predictos are statistically siginificant. 
VIF shows us that RUSH_HOUR has a GVIF of 6.18 and TAGESZEIT 7.26. This confirms that I should not use those predictors and remove them from my model.
"

##Quadratic effects (not sure if include in the final file)
#model with several parameters
lm.delay_hour_temp_precip <- lm(ANKUNFTDELAY_min ~ STUNDE_ANKUNFT + w_temp_avg_c_Luzern + w_precip_mm_Luzern, data = zb_final_subset)
 
lm.delay_hour_temp_precip_quadratic <- update(lm.delay_hour_temp_precip, .~. + I(w_precip_mm_Luzern^2)+ I(w_temp_avg_c_Luzern^2)+ I(STUNDE_ANKUNFT^2))
"I have added here 3 quadratic terms. The model fitts better however it is more complex, danger of overfitting. Maybe a model with just one quadratic term should be considered"

anova(lm.delay_hour_temp_precip, lm.delay_hour_temp_precip_quadratic)
"The Res.Df is lower in the second model by 3 degrees because I added 3 quadratic terms. The RSS decreased which means that the quadratic model explains more variance in the data. 
The F-statistic is quite high which shows that the added quadratic terms significantly improve the model.
The p-value is close to 0 which means that the improvement is statistically highly significant. This suggests that I should use a quadratic model instead of a linear model.
Nevertheless, there is the risk of overfitting because adding too many terms might capture noise rather than relevant patterns"


## Generalised Additive Models GAM

" !!!!! INFORMATION FOR ME: GAM NOT USE FOR COUNT DATA. CATEGORICAL YES BUT WITHOUT SMOOTHER !!!!!"

library(mgcv)

gam_trial_1 <- gam(ANKUNFTDELAY_min ~ s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern) +LINIEN_TEXT + HALTESTELLEN_NAME, data = zb_final_subset) 

summary (gam_trial_1)

plot(gam_trial_1, residuals = TRUE, cex = 2)

"I am not sure if the categorical works witht this gam model. I will try now a simple model to see the effect on the results. "

gam_trial_temp <- gam(ANKUNFTDELAY_min ~ s(w_temp_avg_c_Luzern), data = zb_final_subset) 

summary (gam_trial_temp)

plot(gam_trial_temp, residuals = TRUE, cex = 2)

"p-value is close to 0. Therefore, we have strong evidence that the smooth terme is not equal to 0.
The predictor w_temp_avg_c_Luzern has an effect on ANKUNFTDELAY_min equal to polinominal degree 9. 
However, the R^2 shows just 0.05% meaning that the model doesnt represent well the reality."

gam_trial_precip <- gam(ANKUNFTDELAY_min ~ s(w_precip_mm_Luzern), data = zb_final_subset) 

summary (gam_trial_precip)

plot(gam_trial_precip, residuals = TRUE, cex = 2)

"p-value is close to 0. Therefore, we have strong evidence that the smooth terme is not equal to 0.
The predictor w_precip_mm_Luzern has an effect on ANKUNFTDELAY_min equal to polinominal degree 9. 
However, the R^2 shows just 0.03% meaning that the model doesnt represent well the reality and represents it even less well than with w_temp_avg_c_Luzern."

gam_trial_Abfahrt_Ankunft <- gam(ABFAHRTDELAY_min ~ s(ANKUNFTDELAY_min), data = zb_final) 

summary (gam_trial_Abfahrt_Ankunft)

plot(gam_trial_Abfahrt_Ankunft, residuals = TRUE, cex = 2)

"My idea behind this model: I wanted to see the effect delay arrival has on depature. Of course, the delay arrival has a strong influence on depature delay.
However, often trains can catch up esplicity because of the cap between arrival and depature.Therefore, it would be interesting to know which delay duration on 
arrival has an influence depature delay. When can the train catch up again completly? When just reducing the delay?
I used here the full dataset and not the subset because the model did not depend on a weather region"

"p-value is close to 0. Therefore, we have strong evidence that the smooth terme is not equal to 0.
The predictor ANKUNFTDELAY_min has an effect on ABFAHRTDELAY_min equal to polinominal degree 8. 
The R^2 shows 80% meaning that the model represents well the reality (which is obvious because arrival delay has mostly an influence on depature delay)."

gam_temp_precip_haltestellen <- gam(ANKUNFTDELAY_min ~ HALTESTELLEN_NAME+ s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern), data = zb_final_subset) 

summary(gam_temp_precip_haltestellen)

"
Interpretation parametric coefficients:
Besides the station Stans all the station seem to be statistically significant with p-value close to 0.
The delay at the station Hergiswil Matt is on average 0.89 min with temperature 0C and precipitation at 0mm.
The delay at the station Hergiswil NW is on average -0.1 min less than Hergiswil Matt with temperature 0C and precipitation at 0mm.
The delay at the station Kriens Mattenhof is on average -0.18 min less than Hergiswil Matt with temperature 0C and precipitation at 0mm.
The delay at the station Luzern is on average -0.6 min less than Hergiswil Matt with temperature 0C and precipitation at 0mm.
The delay at the station Luzern Allmend/Messe is on average -0.48 min less than Hergiswil Matt with temperature 0C and precipitation at 0mm.
The delay at the station Stans is not statistically significant different to the delay in Hergiswil Matt.
Interesting enough. The Station Stansstad is the only station which has a larger delay than Hergiswil Matt. The train is on average 0.33 min more delayed with temperature 0C and precipitation at 0mm.

Concerning the smooth terms:
The p-value of both smooth terms w_temp_avg_c_Luzern and w_precip_mm_Luzern are close to 0. Therefore, we have strong evidence that the smooth terms are not equal to 0 and have a significant non-linear effect on ANKUNFTDELAY_min.
The edf of both smooth terms show that they have an effect on ANKUNFTDELAY_min equal and is recommended to use a polinominal degree 9.

R^2 shows that the model represents 9.5% of the reality. This is the highest gam - R^2 so fare. Nevertheless, many varation are still unexplained and may be influenced by other factors.
"
plot(gam_temp_precip_haltestellen, residuals = TRUE, select = 1)

"The effect of temperature on train delays is statistically significant. However, the plot suggests the acutal impact is weak. As the the curve is rather flat, we can assume that temerature is not a strong predictor of ANKUNFTDELAY_min. "

plot(gam_temp_precip_haltestellen, residuals = TRUE, select = 2)

"!!!Plot interpretation here!!!"

gam_temp_precip_rush_hour <- gam(ANKUNFTDELAY_min ~ RUSH_HOUR + s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern), data = zb_final_subset) 

summary(gam_temp_precip_rush_hour)

"
Interpretation parametric coefficients:
Rush_hour_none and rush_hour_vormittag are statistically significant, meaning they have a p-value close to 0. We can assume that both predictor are not equal to the reference category rush_hour_abend. In other words: the 2 predictos differ statistically significant on average from rush_hour_abend.
During evening rush hour (rush_hour_abend) the delay is on average 1.08 min with with temperature 0C and precipitation at 0mm.
If there is no rush hour (rush_hour_none) the delay is 0.5 min than in the evening
During the morning rush hour (rush_hour_vormittag) the delay is 0.28 min less than in the evening. 

The smooth terms are the same as gam_temp_precip_haltestellen. For completeness, the information is provided again below:
The p-value of both smooth terms w_temp_avg_c_Luzern and w_precip_mm_Luzern are close to 0. Therefore, we have strong evidence that the smooth terms are not equal to 0 and have a significant non-linear effect on ANKUNFTDELAY_min.
The edf of both smooth terms show that they have an effect on ANKUNFTDELAY_min equal and is recommended to use a polinominal degree 9.


R^2 shows that the model represents 7.9% of the reality which is less than with the gam_temp_precip_haltestellen model.

The plots are very similar to those for gam_temp_precip_haltestellen; therefore, a separate explanation is not necessary.

"

"Until now we have used ANKUNFTDELAY_min as a response variable. The parametric coefficients were different due to different count and categorical predictors. The smooth terms and the plot from the different models were similar though. 
Lets have now a look at GAM models with ABFAHRTDELAY_min as response variable with different predictors. "

gam_temp_precip_tageszeit <- gam(ABFAHRTDELAY_min ~ TAGESZEIT  + s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern), data = zb_final_subset) 

summary(gam_temp_precip_tageszeit)

plot(gam_temp_precip_tageszeit, residuals = TRUE, select = 1)

plot(gam_temp_precip_tageszeit, residuals = TRUE, select = 2)



"Interpretation parametric coefficients:
Morning, lunch, afternoon, night predictos are statistically significant. All those predictors' p-value besides the morning predictor are close to 0. The morning's p-value is with 0.031 still significant.
This means that all those predictors differ statistically significant on average from their reference category which is evening.
Lets have a more detailed look:
In the evening the delay of ZB trains in the region of Lucerne is on average 0.75 min if the temperature is 0C and precipitation is 0mm.
During lunch time the trains are on average 0.46 min less delayed than in the evening, assuming constant temperature and precipitation. 
In the afternoon, the trains are on average 0.28 min less delayed than in the evening, assuming constant temperature and precipitation. 
During the night, the trains are on average 0.37 min less delayed than in the evening, assuming constant temperature and precipitation.
The train delay in the morning is 0.06 minutes less than in the evening, although this difference is smaller and statistically significant at p = 0.0306 (indicating it is still worth noting, but the effect is small compared to other times of day).


The smooth terms and plots are the same as gam_temp_precip_haltestellen; therefore, a separate explanation is not necessary.

R^2 shows that the model represents 7.6% of the reality which is less than gam_temp_precip_haltestellen and gam_temp_precip_rush_hour.
"

"We have seen so fare different GAM models and realized that the smooth terms and plots hardly changed. 
The challenge with our data is that we have several count and categorical predictors but just a few continous predictors. 
Lets have now a look at a bit more complex model with several count and categorical predictors. Will the smooth terms and the plot change even though we have the same continous predictors?"




gam_temp_precip_tageszeit_linien_text <- gam(ABFAHRTDELAY_min ~ TAGESZEIT + LINIEN_TEXT + s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern), data = zb_final_subset) 

summary(gam_temp_precip_tageszeit_linien_text)

plot(gam_temp_precip_tageszeit_linien_text, residuals = TRUE, select = 1)
plot(gam_temp_precip_tageszeit_linien_text, residuals = TRUE, select = 2)


"Interpretation parametric coefficients
The reference level for the categorical predictor TAGESZEIT is Abend (Evening), and for LINIEN_TEXT, it is EXT.
The EXT line of Zentralbahn (ZB) in the Lucerne region has an average departure delay of 2.03 minutes during the evening, assuming a temperature of 0°C and no precipitation (0 mm).

Effect of Time of Day (TAGESZEIT) on Departure Delays keeping all other variables constant:

Departure delays vary significantly based on the time of day.
During lunch (Mittag), trains experience on average 0.52 minutes less delay compared to the evening.
In the afternoon (Nachmittag), departure delays are on average 0.31 minutes lower than in the evening.
During night (Nacht) operations, trains have on average 0.42 minutes less delay compared to the evening.
In the morning (Vormittag), delays are on average 0.09 minutes lower than in the evening.

All the predictors have p-values smaller than 0.05, indicating that they have a significant effect on departure delay. Furthermore, the predictors lunch, afternoon, night, and morning differ significantly from evening, meaning their differences are not equal to zero.

Effect of Train Lines (LINIEN_TEXT) on Departure Delays:

The departure delays of various train lines are compared against the reference category EXT under the assumption of evening time, 0°C temperature and no precipitation:
The IR line has on average 0.67 minutes less delay than EXT, but this difference is not statistically significant (p = 0.14). This means we cannot conclude that the IR line experiences fewer or more delays than EXT.
The PE line experiences on average 3.12 minutes less delay compared to EXT (p = 0.039), meaning it has significantly fewer delays.
The S4 line has on average 1.34 minutes less delay compared to EXT (p = 0.003). This difference is statistically significant.
The S41 line experiences on average 2.02 minutes less delay than EXT (p < 0.001), indicating a strong reduction in departure delays.
The S44 line has on average 1.57 minutes less delay than EXT (p = 0.0007), showing a statistically significant improvement.
The S5 line exhibits on average 1.09 minutes less delay than EXT (p = 0.017), confirming a statistically significant difference.
The S55 line shows the largest reduction, with on average 2.70 minutes less delay than EXT (p < 0.001), making it the most punctual train line relative to EXT.
All the LINIEN_TEXT predictors, except for IR, have p-values smaller than 0.05, indicating that they have a significant effect on departure delay. This suggests that the departure delays for the PE, S4, S41, S44, S5, and S55 lines differ significantly from the EXT line, meaning their differences are not equal to zero. In contrast, the IR line does not show a statistically significant difference from the EXT line.


The adjusted R^2 for this model is 0.0979, indicating that approximately 9.88% of the variance in departure delay (ABFAHRTDELAY_min) is explained by the predictors TAGESZEIT, LINIEN_TEXT, w_temp_avg_c_Luzern, and w_precip_mm_Luzern.
It is the highest R^2 compared to the other GAM models.
While this value suggests that a large proportion (~90%) of the variability in train departure delays remains unexplained, this is expected in real-world transportation data. Delays are often influenced by unobserved factors such as operational constraints, temporary disruptions, infrastructure issues, or human decision-making, which are not included in the model.
Despite the relatively low R², the model still provides valuable insights into significant predictors of train delays. The results confirm that time of day (TAGESZEIT), train line (LINIEN_TEXT), temperature, and precipitation all have statistically significant effects on departure delays, contributing to a better understanding of the factors influencing punctuality.
"

"Although the model includes multiple predictors, the smooth terms and their respective plots remain consistent with those observed in the gam_temp_precip_haltestellen model. 
This indicates that while additional categorical variables enhance the model's explanatory power, the overall influence of temperature and precipitation on departure delay follows 
a similar pattern. By incorporating various predictor variables, we have effectively explored all possible ways to utilize Generalized Additive Models (GAM) with our dataset, 
ensuring a comprehensive analysis of the factors influencing train delays.
"





"Todo: Abfahrt Delay min // Linien Text //  Tageszeit. Noch einmal durchgehen View. Evtl habe ich dann noch eine andere Idee 
Am Schluss ggf. notieren, welches Model ich vorschlagen würde, in bezug vielleicht zum R^2"


#Ramon codes

mm <- zb_final_subset$w_temp_avg_c_Luzern

summary(mm)

hist(zb_final_subset$w_precip_mm_Luzern)

sum(mm==37.1)

factor(mm)

