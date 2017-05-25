## Loading the functions
source("../Functions/sanitizing.R")
source("../Functions/timespines_fun.R")
## Getting the datasets
source("LoadingDatasets.R") ## Datasets



## Getting the data in the right format
extant_data <- lapply(datasets[[1]][-6], data.timespine, standardise = TRUE) #TG: excl europe

## Plotting the distribution of body masses
op <- par(mfrow = c(3,3))
for(zone in 1:length(extant_data)) {
    plot.timespine(extant_data[[zone]], main = names(extant_data[zone]), xlab = "Standard deviations")
}
par(op)


## Testing the differences
lapply.test <- function(one_site, test = wilcox.test) {
    return(test(one_site$measure[which(one_site$armours)], one_site$measure[which(!one_site$armours)]))
}

testings <- lapply(extant_data, lapply.test, test = wilcox.test)

op <- par(mfrow = c(3,3))
for(zone in 1:length(extant_data)) {
    boxplot(extant_data[[zone]]$measure[which(extant_data[[zone]]$armours)], extant_data[[zone]]$measure[which(!extant_data[[zone]]$armours)], ylab = "sd", xaxt = "n", main = names(extant_data[zone]))
    text(1.5, 3, paste("p value:", round(testings[[zone]]$p.value, digit = 3)))
    axis(1, at = 1:2, labels = c("Armour", "No armour"))
}
par(op)
