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
        rows <- data[,7+(body.part[[1]]*4)] == 1
        if(length(body.part) > 1) {
            for(part in 2:length(body.part)) {
                rows <- rows & data[,7+(body.part[[part]]*4)] == 1
            }
        }
    }

    ## Replacing the body parts by the column number
    body.part <- as.numeric(gsub(1, 11, body.part))
    body.part <- as.numeric(gsub(2, 15, body.part))
    body.part <- as.numeric(gsub(3, 19, body.part))

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



#' @title Timespine data
#'
#' @description Getting the right body measurement and armours occurences
#'
#' @param one_site One site in the proper format
#' @param measure The column where the body measurement is (length or mass) (default = length (\code{7}))
#' @param standardise Whether to get the measurements in terms of standard deviations (\code{TRUE}) or node (\code{FALSE}, default)
#' 
#' @examples
#' 
#' @author Thomas Guillerme
#' 
data.timespine <- function(one_site, measure = 7, standardise = FALSE) {
    ## Selecting the predators
    carnivores <- grep("carnivore", one_site[, 10], ignore.case = TRUE)

    ## Selecting the biggest predator
    predator <- max(one_site[carnivores,measure], na.rm = TRUE)

    ## Correcting for the predator's size
    Body_measure <- one_site[,measure]/predator

    if(standardise) {
        ## Get the mean and sd
        mean <- mean(Body_measure, na.rm = TRUE)
        sd <- sd(Body_measure, na.rm = TRUE)

        ## Correct the values in terms of units of sd
        Body_measure <- sqrt(((mean - Body_measure)/sd)^2)
    }

    ## Get the spiny-ness
    armours <- get.armour(one_site)

    return(list("measure" = Body_measure, "armours" = armours))
}



#' @title Timespine density
#'
#' @description Calculating the data density
#'
#' @param timespine_data A list containing "measure" and "armour"
#' 
#' @examples
#' 
#' @author Thomas Guillerme
#' 
density.timespine <- function(timespsine_data) {
    ## Calculating the histogram
    hist <- hist(timespsine_data$measure, plot = FALSE)
    ## Calculating the
    density <- density(timespsine_data$measure, na.rm = TRUE)
    ## Scaling the density to match with the histogram
    density$y <- density$y * hist$counts[1] / hist$density[1]

    return(list("histogram" = hist, "density" = density))
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



#' @title plots the results
#'
#' @description Plots the results
#'
#' @param site_measurement Measurements for on site in timespines format
#' @param display.armour Whether to display the armours
#' @param xlab x label (default = "Body Length (m)")
#' @param ... arguments to be passed to plot
#' 
#' @examples
#' 
#' @author Thomas Guillerme
#' 

plot.timespine <- function(site_measurements, display.armour = FALSE, xlab = "Body Length (m)", ...) {
    ## Calculating the density
    density <- density.timespine(site_measurements)

    ## Plotting the histogram and the density curve
    plot(density$histogram, xlab = xlab, ylab = "Density", border = "grey", ...)
    lines(density$density)

    if(display.armour) {
        ## Adding the lines under the curve
        for(one_BL in 1:length(site_measurements$measure[which(site_measurements$armours)])) {
            ## Get the measurement of the armoured thing
            measure <- site_measurements$measure[which(site_measurements$armours)][one_BL]
            if(!is.na(measure)) {
                ## Find the closest x
                x_value <- narrow.down.x(measure, density$density$x)
                ## Plot the segment
                segments(x0 = density$density$x[x_value], y0 = 0, y1 = density$density$y[x_value], lwd = 2)
            }
        }
    }
    return(invisible())
}
