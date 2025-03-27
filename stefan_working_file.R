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
      x = ABFAHRTDELAY_sec, 
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