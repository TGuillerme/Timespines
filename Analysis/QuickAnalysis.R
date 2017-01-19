## Loading the data
one_site <- read.csv("../Data/GreenBassin.csv")

## Running the dummy analysis (using body length instead of body mass for now)


## Get the max predator (here I'm just assuming it's the biggest guy)
predator <- max(one_site[,5])

## Correct for the predator's size
#Body_length <- one_site[,5]/predator

## Get the spiny-ness (if the score is > 0, it means at least one part of the body has armour)
get.armour <- function(species, data) {
    ## Get the "armourness" of the body parts
    head <- ifelse(data[species, 8] == 1, sum(data[species, 9:10]), NA)
    body <- ifelse(data[species, 12] == 1, sum(data[species, 13:14]), NA)
    tail <- ifelse(data[species, 16] == 1, sum(data[species, 17:18]), NA)
    ## Get the average "armourness score"
    return(mean(c(head, body, tail), na.rm = TRUE))
}
armours <- unlist(lapply(as.list(seq(1:ncol(one_site))), get.armour, data = one_site))

## Make it binary
armours <- ifelse(armours != 0, 1, 0)

# ## Density plot
# library(hdrcde)
# hdr.den(Body_length, xlab = "Body Length (m)", ylab = "Density", main = "Occurrences of armoured fishes")
# hist(Body_length[which(armours == 1)], col = "orange", breaks = 50, add = TRUE)


## Calculating the histogram
hist_BL <- hist(Body_length, plot = FALSE)
## Calculating the
density_BL <- density(Body_length)
## Scaling the density to match with the histogram
density_BL$y <- density_BL$y * hist_BL$counts[1] / hist_BL$density[1]

## Plotting the histogram and the density curve
plot(hist_BL, xlab = "Body Length (m)", ylab = "Density", main = "Occurrences of armoured fishes", border = "grey") ; lines(density_BL)

## Function for narrowing down the value of BL under the curve
narrow.down.x <- function(x, density) {
    ## Start from rounding = 0
    rounding <- 0
    ## Check if any matching with lower rounding
    while(length(which(round(x, digit = rounding) == round(density, digit = rounding))) != 0) {
        rounding <- rounding + 1
    }
    ## Return the before last rounding
    narrow <- which(round(x, digit = rounding-1) == round(density, digit = rounding-1))
    return(narrow[ceiling(length(narrow)/2)])
}

## Adding the lines under the curve
for(one_BL in 1:length(Body_length[which(armours == 1)])) {
    x_value <- narrow.down.x(Body_length[which(armours == 1)][one_BL], density_BL$x)
    segments(x0 = density_BL$x[x_value], y0 = 0, y1 = density_BL$y[x_value], lwd = 2)
}


## Testing the differences
testing <- t.test(Body_length[which(armours == 1)], Body_length[which(armours == 0)])
par(bty = "n")
boxplot(Body_length[which(armours == 1)], Body_length[which(armours == 0)], ylab = "Body Length", xaxt = "n", main = "
    Difference between groups")
text(1.5, 1, paste("p value:", round(testing$p.value, digit = 3)))
axis(1, at = 1:2, labels = c("Armour", "No armour"))