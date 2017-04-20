source("sanitizing.R")

#' @title Get armour data
#'
#' @description Get the taxa with the different type of armour
#'
#' @param data A data frame or matrix using the timespines data collection format
#' @param body.part \code{0} for any, \code{1} for the head, \code{2} for the body and \code{3} for the tail (or any combination of them).
#' @param armour.type \code{0} for any armour type, \code{1} for projections, \code{2} for surfaces (or any combination of them)
#' 
#' @details
#' To get projection AND surfaces for head AND tail, use \code{get.armour(data, body.part = c(1,3), armour.type = c(1,2))}.
#' 
#' @examples
#' 
#' @author Thomas Guillerme
#' 

get.armour <- function(data, body.part = 0, armour.type = 0) {

    ## Sanitizing
    check.class(data, c("matrix", "data.frame"))
    check.class(body.part, c("numeric", "integer"))
    if(any(body.part > 3)) stop("body.part should be an integer between 0 and 3.")
    check.class(armour.type, c("numeric", "integer"))
    if(any(armour.type > 2)) stop("body.part should be an integer between 0 and 2.")

    ## Function for fetching any 1s from the data frame
    get.armour.fun <- function(row, part, type, data) {
        return(ifelse(data[row, (part+type)] == 1, TRUE, FALSE))
    }

    ## Setting the body parts/armour types to analysis
    if(any(armour.type == 0)) armour.type <- c(1,2)
    if(any(body.part == 0)) {
        body.part <- c(1,2,3)
        rows <- rep(TRUE, nrow(data))
    } else {
        ## Select only the data with the present body parts
        rows <- data[,4+(body.part[[1]]*4)] == 1
        if(length(body.part) > 1) {
            for(part in 2:length(body.part)) {
                rows <- rows & data[,4+(body.part[[part]]*4)] == 1
            }
        }
    }

    ## Replacing the body parts by the column number
    body.part <- as.numeric(gsub(1, 8, body.part))
    body.part <- as.numeric(gsub(2, 12, body.part))
    body.part <- as.numeric(gsub(3, 16, body.part))

    ## Initialising the results list
    results <- list(logical())
    counter <- 0

    ## Looping through the body parts
    for(part in 1:length(body.part)) {
        for(type in 1:length(armour.type)) {
            ## Increment the counter
            counter <- counter + 1
            ## Get the armoured things
            results[[counter]] <- unlist(lapply(as.list(1:nrow(data)), get.armour.fun, body.part[[part]], armour.type[[type]], data))
        }
    }

    ## Combine the data
    output <- results[[1]]
    if(length(results) > 1) {
        for(one_res in 2:length(results)) {
            output <- results[[one_res]] | output
        }
    } 

    ## Selecting only the rows of interest
    output <- output & rows

    return(output)

}

#' @title Narrow down x
#'
#' @description Getting the most precise value of x to trace a line under a density curve.
#'
#' @param x The position on the x axis
#' @param density The density curve
#' 
#' @examples
#' 
#' @author Thomas Guillerme
#' 

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
