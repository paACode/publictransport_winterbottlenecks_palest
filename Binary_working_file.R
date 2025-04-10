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

#Reducing the subbset just with the relevant columns
zb_final_binominal <- zb_final_binominal %>% select(BETRIEBSTAG, LINIEN_TEXT, FAELLT_AUS_TF, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, AN_PROGNOSE_STATUS, ABFAHRTSZEIT, AB_PROGNOSE, AB_PROGNOSE_STATUS, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, Train_Delayed, TAGESZEIT, RUSH_HOUR, Train_RUSH_HOUR, STUNDE_ANKUNFT)


#Removing rows NA in ANKUNFTDELAY_min

zb_final_binominal <- zb_final_binominal %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_binominal$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_binominal <- zb_final_binominal %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_binominal$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0


str(zb_final_binominal)

View(zb_final_binominal)


#Models trial

glm_trial <- glm(Train_Delayed ~ LINIEN_TEXT, family = "binomial", data = zb_final_binominal)

summary(glm_trial)

ggplot( data = zb_final_binominal, mapping = aes(y =Train_Delayed, x = LINIEN_TEXT )) + geom_point ()


glm_trial_2 <- glm(Train_RUSH_HOUR ~ ANKUNFTDELAY_min, family = "binomial", data = zb_final_binominal)

summary(glm_trial_2)

"The p-value for ANKUNFTDELAY_min is very small, essentially less than 2e-16. This indicates that the arrival delay (ANKUNFTDELAY_min) has a statistically significant effect on whether a train runs during rush hour (Train_RUSH_HOUR).
Since the coefficient for ANKUNFTDELAY_min is positive (0.101845), it suggests that as the arrival delay increases, the likelihood of the train running during rush hour also increases.

Number of Fisher Scoring iterations is 4 which is an acceptable value. 
"

# Convert the Train_RUSH_HOUR to numeric (0 = FALSE, 1 = TRUE) otherwise we can not use it with a logistic regression model
zb_final_binominal$Train_RUSH_HOUR <- as.numeric(zb_final_binominal$Train_RUSH_HOUR)

#Plotting the model

ggplot(data = zb_final_binominal, mapping = aes(x = ANKUNFTDELAY_min, y = Train_RUSH_HOUR)) + 
  geom_point() + 
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, 
              color = "darkblue") +  # Add logistic regression curve
  labs(title = "Logistic Regression Fit", 
       x = "Arrival Delay (minutes)", 
       y = "Train Rush Hour Probability") 

"Higher arrival delay are assoicated with higher probabilities that the train is delayed at arrival"

#Discretise the fitted value in order to be able to compare the observed and fitted value

fitted.glm_trial_2_disc <- ifelse(fitted(glm_trial_2) < 0.5, yes = 0, no = 1)
head(fitted.glm_trial_2_disc)

#Comparing observed and fitted value

d.obs.fit.glm_trial_2 <- data.frame(obs=zb_final_binominal$Train_RUSH_HOUR, fitted = fitted.glm_trial_2_disc)

#Comparing the observed and the discretised fitted values


table(d.obs.fit.glm_trial_2$obs, d.obs.fit.glm_trial_2$fitted)


"This is a confusion matrix comparing the observed values (obs = actual outcomes) to the discretized fitted values 
(fitted = predicted outcomes after applying the 0.5 threshold to the logistic regression model).
41170: These are the instances where the actual outcome (Train_RUSH_HOUR = 0) matched the model's prediction (Train_RUSH_HOUR = 0). In other words, these are the cases where the train did not run during rush hour, and the model correctly predicted this
These are the cases where the actual outcome was Train_RUSH_HOUR = 0 (train did not run during rush hour), but the model incorrectly predicted that it would run (Train_RUSH_HOUR = 1).
15796: These are the cases where the actual outcome was Train_RUSH_HOUR = 1 (train did run during rush hour), but the model incorrectly predicted that it would not run (Train_RUSH_HOUR = 0).
74: These are the instances where the actual outcome was Train_RUSH_HOUR = 1 (train did run during rush hour), and the model correctly predicted this (Train_RUSH_HOUR = 1).
"

#Performance Metrics:

# True Positives (TP), False Positives (FP), False Negatives (FN), True Negatives (TN)
TP <- 74
FP <- 179
FN <- 15796
TN <- 41170

# Calculate Accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)
accuracy

"The percentage of correct predictions (both true positives and true negatives). So, the model correctly predicted about 71% of the time."

# Calculate Precision
precision <- TP / (TP + FP)
precision

"Precision (for positive class): The proportion of positive predictions that were actually correct. So, when the model predicted a train would run during rush hour, it was correct about 29.3% of the time."

# Calculate Recall
recall <- TP / (TP + FN)
recall

"Recall (for positive class): The proportion of actual positive cases that the model correctly identified. So, the model identified only 0.47% of the actual positive cases (train running during rush hour)."

"Observations:
The model performs fairly well at predicting the negative class (trains not running during rush hour), as shown by the high number of true negatives (41170).
However, it struggles with predicting the positive class (trains running during rush hour). The recall is very low (around 0.47%), meaning the model is not very good at identifying the rare positive cases.

Conclusion:
The logistic regression model developed to predict whether a train will run during rush hour (Train_RUSH_HOUR) based on the arrival delay (ANKUNFTDELAY_min) shows that ANKUNFTDELAY_min has a statistically significant impact on the likelihood of a train running during rush hour. Specifically, the model's coefficient for ANKUNFTDELAY_min is positive (0.101845), which indicates that as the arrival delay increases, the probability of the train running during rush hour also increases.
The performance metrics of the model suggest moderate predictive power, with an accuracy of 71%. However, the precision and recall values indicate that while the model correctly predicts most cases where a train is not running during rush hour (true negatives), it struggles with identifying trains that are running during rush hour (true positives). This imbalance in the model's performance, reflected in a low recall (0.0047), suggests that the model is more successful at predicting when a train is not in rush hour rather than accurately predicting when a train will be running during rush hour.
The relationship between ANKUNFTDELAY_min and Train_RUSH_HOUR is evident: a higher ANKUNFTDELAY_min increases the likelihood of a train running during rush hour. This aligns with common operational patterns, such as more trains operating during higher traffic periods like rush hour.
To improve model performance, especially in predicting rush hour trains (positive class), further exploration could include adding additional predictors, addressing class imbalance, or employing more advanced modeling techniques.
"

#Models Binary delay

# Convert the Train_Delayed to numeric (0 = FALSE, 1 = TRUE) otherwise we can not use it with a logistic regression model
zb_final_binominal$Train_Delayed <- as.numeric(zb_final_binominal$Train_Delayed)

str(zb_final_binominal)


#Plotting the data

zb_final_binominal %>%
  group_by(LINIEN_TEXT) %>%
  summarise(ProportionDelayed = mean(Train_Delayed, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LINIEN_TEXT, ProportionDelayed), y = ProportionDelayed)) +
  geom_point(size = 3, color = "steelblue") +
  labs(x = "Train Line", y = "Proportion Delayed") +
  theme_minimal()

"In this plot, we can see that the EXT line has the highest proportion of delayed trains, with approximately 40% 
of its services experiencing delays. The line with the second highest delay rate is the R71, where around 24% of trains are delayed. 
On the other end of the spectrum, the S41 line shows the lowest delay rate, with only about 10% of its trains being delayed.
"

"To predict whether a train is delayed, we use a binomial logistic regression with Train_Delayed as the binary response variable 
and LINIEN_TEXT (train line) as the predictor. This model estimates how the probability of delay varies across different train lines.
"

glm_delay <- glm(ANKUNFTDELAY_min ~ LINIEN_TEXT, family = "binomial", data = zb_final_binominal)

summary(glm_delay)

"The number of Fisher Scoring iterations is 7, which is an acceptable value.
On one hand, the p-values for almost all the train lines are very small, indicating that the train lines have a statistically significant 
effect on whether a train is delayed or not. Since the coefficients for the train lines are negative, it suggests that the different train lines have a 
lower probability of being delayed compared to the baseline train line.
On the other hand, the train line R71, which operates between Meiringen and Innertkirchen, has a p-value above 0.05. This suggests that R71 does not have 
a statistically significant impact on whether a train is delayed or not. This is interesting because, when the predictors were plotted, R71 was the line with 
the second-highest probability of delay, with an average delay of around 24%.
The reason for this contradiction could be that while the R71 line might often experience delays, the variance of the delays might be quite narrow. To explore this, 
let's take a look at the boxplot of the train delays by line.
"

boxplot(ANKUNFTDELAY_min ~ LINIEN_TEXT , data = zb_final_binominal)

"In the boxplot, we can see that the R71 has a relatively large variance between the 25th and 75th percentiles of the data. 
Nevertheless, R71 has fewer outliers compared to the other lines. The delays on R71 might be consistent, but not extreme 
enough to be detected as a significant predictor in the logistic regression model.
Just because R71 has a relatively high average delay doesn't necessarily mean that the line itself significantly affects the likelihood of delays 
when considering all other factors. The logistic regression model is trying to predict the probability of a delay (yes/no) based on various predictors. 
Other variables—such as weather, time of day, or operational factors—might influence whether a delay occurs for R71 trains.e logistic regression model is trying to predict 
the probability of delay (yes/no) based on various predictors. Other variables (like weather, time of day, or other train line-related factors) might influence whether a delay occurs for R71 trains. 
"


#############################

ggplot(data = zb_final_binominal, mapping = aes(x = LINIEN_TEXT, y = predicted_prob)) + 
  geom_point(alpha = 0.4, color = "darkblue") + 
  labs(title = "Predicted Probability of Train Delay by Train Line", 
       x = "Train Line", 
       y = "Predicted Probability of Delay") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

