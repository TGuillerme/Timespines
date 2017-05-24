## Loading the functions
source("../Functions/sanitizing.R")
source("../Functions/timespines_fun.R")
## Getting the datasets
source("LoadingDatasets.R") ## Datasets



## Getting the data in the right format
one_site <- data.timespine(datasets[[1]])


plot.timespine(one_site)

## Testing the differences
testing <- t.test(one_site$measure[which(one_site$armours)], one_site$measure[which(!one_site$armours)])

boxplot(one_site$measure[which(one_site$armours)], one_site$measure[which(!one_site$armours)], ylab = "Body Length", xaxt = "n", main = "Difference between groups")
text(1.5, 1, paste("p value:", round(testing$p.value, digit = 3)))
axis(1, at = 1:2, labels = c("Armour", "No armour"))





####Kevin Extra analysis based on the distance a body mass is from the entire mean body mass
###as we expect spines to be intermediate than non-armoured species should show higher 
###distances from the overall mean body size.

##get the mean and standard deviation of the entire group
mean_bl <- mean(Body_length)
SD_bl <- sd(Body_length)

#get the distance from the mean in terms of SD units
z_dif_arm <- (((mean_bl - Body_length[which(armours)])/SD_bl)^2)^0.5
z_dif_noarm <- (((mean_bl - Body_length[which(!armours)])/SD_bl)^2)^0.5

Ztesting <- t.test(z_dif_arm, z_dif_noarm)

#plots
boxplot(z_dif_arm, z_dif_noarm, ylab = "Body Length", xaxt = "n", main = "
        Difference between groups")
text(1.5, 1, paste("p value:", round(testing$p.value, digit = 3)))
axis(1, at = 1:2, labels = c("Armour", "No armour"))




