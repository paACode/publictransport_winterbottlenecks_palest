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
