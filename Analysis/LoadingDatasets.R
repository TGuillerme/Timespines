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