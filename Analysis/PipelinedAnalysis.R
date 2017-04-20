## Pipelined analysis following the QuickAnalysis.R format

one_site <- read.csv("../Data/Price_ExtantData.csv")

## Removing the source column
one_site[,5] <- NULL

## Running the dummy analysis (using body length instead of body mass for now)

## Selecting the biggest fish
predator <- max(one_site[,5])

## Correcting for the predator's size
Body_length <- one_site[,5]/predator

## Get the spiny-ness
armours <- get.armour(one_site)

## Calculating the histogram
hist_BL <- hist(Body_length, plot = FALSE)
## Calculating the
density_BL <- density(Body_length)
## Scaling the density to match with the histogram
density_BL$y <- density_BL$y * hist_BL$counts[1] / hist_BL$density[1]


## Plotting the histogram and the density curve
plot(hist_BL, xlab = "Body Length (m)", ylab = "Density", main = "Occurrences of armoured fishes", border = "grey") ; lines(density_BL)

## Adding the lines under the curve
for(one_BL in 1:length(Body_length[which(armours)])) {
    x_value <- narrow.down.x(Body_length[which(armours)][one_BL], density_BL$x)
    segments(x0 = density_BL$x[x_value], y0 = 0, y1 = density_BL$y[x_value], lwd = 2)
}

## Testing the differences
testing <- t.test(Body_length[which(armours)], Body_length[which(!armours)])

boxplot(Body_length[which(armours)], Body_length[which(!armours)], ylab = "Body Length", xaxt = "n", main = "
    Difference between groups")
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
z_dif_noarm <-  (((mean_bl - Body_length[which(!armours)])/SD_bl)^2)^0.5

Ztesting <- t.test(z_dif_arm, z_dif_noarm)

#plots
boxplot(z_dif_arm, z_dif_noarm, ylab = "Body Length", xaxt = "n", main = "
        Difference between groups")
text(1.5, 1, paste("p value:", round(testing$p.value, digit = 3)))
axis(1, at = 1:2, labels = c("Armour", "No armour"))




