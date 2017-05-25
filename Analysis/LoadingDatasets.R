## Script for cleaning the datasets

## Which datasets to use?
datasets_names <- c(
    "Price_ExtantData",
    "GreenBassin")

## Headers should be
headers_names <- c(
    "Name",           #1 a
    "Group",          #2 b
    "Oldest Age",     #3 c
    "Youngest Age",   #4 d
    "Environement",   #5 e
    "Source",         #6 f
    "Body length",    #7 g
    "Body mass",      #8 h
    "Source",         #9 i
    "Diet",           #10 j
    "Head presence",  #11 k 
    "Head projection",#12 l
    "Head surface",   #13 m
    "Head function",  #14 n
    "Body presence",  #15 o
    "Body projection",#16 p
    "Body surface",   #17 q
    "Body function",  #18 r
    "Tail presence",  #19 s
    "Tail projection",#20 t
    "Tail surface",   #21 u
    "Tail function")  #22 v

## Load the datasets
read.all.data <- function(chain) {
    return(read.csv(paste("../Data/", chain, ".csv", sep = "")))
}
datasets <- lapply(as.list(datasets_names), read.all.data)



###################################
# Sorting extant dataset into zones
###################################


## Split the extant dataset into geographic zone
zones <- c(
    "Ind", #India/Indo/Indian
    "Atlantic",
    "Pacific",
    "Arctic",
    "Africa",    #freshwater
    "Europe",    #freshwater
    "America",   #freshwater
    "Asia",      #freshwater
    "Australia") #freshwater

## Creating the matrix for collecting the data
zones_matrix <- matrix(data = 0, ncol = length(zones), nrow = nrow(datasets[[1]]))
colnames(zones_matrix) <- zones

## Looping through the zones
for(zone in 1:length(zones)) {
    zones_matrix[grep(zones[zone], datasets[[1]][,25], ignore.case = TRUE), zone] <- 1
}

## Getting the unmatched rows
no_match <- which(apply(zones_matrix, 1, sum) == 0)
# datasets[[1]][no_match,25]

## Special categories
special_names <- c(
    "tropical",      # Ind, Pacific, Atlantic
    "global",        # Ind, Pacific, Atlantic, Arctic    #TG: distinguishing circumglobal?
    "all oceans",    # Ind, Pacific, Atlantic, Arctic
    "worldwide",     # Ind, Pacific, Atlantic, Arctic
    "amazon",        # America
    "northern hemisphere", # Pacific, Pacific, Atlantic
    "southern hemisphere", # Ind Pacific Atlantic
    "chile",         # America
    "china",         # Asia
    "US"            # America
    )

special_zones <- list(c(1,2,3), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(7), c(2,3,4), c(1,2,3), c(7), c(8), c(7))

## Looping through the zones
for(zone in 1:length(special_zones)) {
    zones_matrix[grep(special_names[zone], datasets[[1]][,25], ignore.case = TRUE), special_zones[[zone]]] <- 1
}

## Creating the zones subdataset
extant_zones <- list()
for(zone in 1:length(zones)) {
    extant_zones[[zone]] <- datasets[[1]][which(zones_matrix[,zone] == 1), ]
}
names(extant_zones) <- zones
datasets[[1]] <- extant_zones

