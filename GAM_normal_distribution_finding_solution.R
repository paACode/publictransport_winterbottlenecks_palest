
"In the GAM model we need continous data as response variable. In our data set we do not have a wide range of continous data. This is way I will focus
on ANKUNFTDELAY_min and ABFAHRTDELAY_min as response variables.

In order to use ANKUNFTDELAY_min and ABFAHRTDELAY_min as response variable we have to assume that they are normally distributed. Lets have a look at that
"

par(mfrow = c(1, 2))


hist(zb_final_subset$w_temp_avg_c_Luzern, main = "Histogram of ANKUNFTDELAY_min", xlab = "Delay (min)", col = "lightblue", breaks = 30)

hist(zb_final_subset$w_precip_mm_Luzern, main = "Histogram of ABFAHRTDELAY_min", xlab = "Delay (min)", col = "lightblue", breaks = 30)

"The histograms look roughly bell-shaped.
Lets do a second check with QQ plot.
"


qqnorm(zb_final_subset$w_temp_avg_c_Luzern, main = "QQ Plot of ANKUNFTDELAY_min")
qqline(zb_final_subset$w_temp_avg_c_Luzern, col = "red")

qqnorm(zb_final_subset$w_precip_mm_Luzern, main = "QQ Plot of ABFAHRTDELAY_min")
qqline(zb_final_subset$w_precip_mm_Luzern, col = "red")