#' @title Taxonomy change
#'
#' @description Detects state changes in the taxonomy of a group
#'
#' @param group The name of the group
#' @param group.level The number of the column in data were the group information sits
#' @param state the state to evaluate
#' @param data the dataset
#' @param data.out the column in the dataset containing the data to output (set to 0 for rownames - default)
#' 
#' @examples
#' 
#' @return A \code{list} composed of the following elements:
#' \begin{itemize}
#'  \item \code{state.origin} the origin state (for the direction of the change)
#'  \item \code{changed.taxa} the taxa that have changed within the groups
#'  \item \code{group.taxa} all the taxa from the group
#'  \item \code{changed.val} the changed taxa values
#'  \item \code{group.val} the group values
#' \end{itemize}
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

taxonomy.change <- function(group, group.level, state, data, data.out = 0) {

    ## Getting the members of the group in the data
    group_taxa <- which(data[, group.level] == group)

    ## Getting the average group state
    state_origin <- dispRity::mode.val(state[group_taxa])

    ## Getting the taxa that have not the state_origin
    changed_taxa <- which(state[group_taxa] != state_origin)

    if(length(changed_taxa) == 0) {
        changed_taxa <- NULL
    }

    ## Getting the values out
    if(data.out == 0) {
        data_out <- rownames(data)
    } else {
        data_out <- data[, data.out]
    }

    ## Getting the changed and group values
    changed_val <- data_out[changed_taxa]
    group_val <- data_out[group_taxa]

    return(list( "state.origin" = state_origin, "changed.taxa" = changed_taxa, "group.taxa" = group_taxa, "changed.val" = changed_val, "group.val" = group_val))
}

#' @title Extract change
#'
#' @description Extract the change data from an output list of taxonomy.change, topology.change and branch.length.change
#'
#' @param list The list
#' @param what Which element to output ("changed.taxa", "state.origin", "group.taxa")
#' @param inc.null whether to include groups with no changes (\code{TRUE}), or only changed groups (\code{FALSE} - default)
#'
#' @examples
#' 
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

extract.change <- function(list, what, inc.null = FALSE) {

    ## Output list
    list_out <- lapply(list, function(X, what) return(X[[what]]), what = what)

    if(inc.null) {
        return(list_out)
    } else {
        ## Getting the groups with no changes
        no_changes <- unlist(lapply(list, function(X) any(unlist(lapply(X, is.null)))))

        ## Removing them from the list out
        return(list_out[!no_changes])
    }
}

#' @title Plots changes
#'
#' @description Plots the data change for a single group or a list of groups
#'
#' @param timespines The timespines data output from \code{run.timespines}
#' @param histogram logical, whether to plot the histogram in the background (TRUE)
#' @param scale.density logical, whether to scale the density (TRUE)
#' @param change logical, whether to display the changes as an "hist" or "lines" or both (default)
#' @param col.change a vector of colours for differentiating the changes (default is \code{c("orange", "blue")})
#' @param legend logical, whether to display the legend for the changes colours
#' @param ... Arguments to be passed to plot()
#'
#' @examples
#' 
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.change <- function(timespines.data, histogram = TRUE, scale.density = TRUE, change = c("hist", "lines"), col.change = c("orange", "blue"), legend = TRUE, ...) {

    group.data <- timespines.data$normal.val
    change.data <- timespines.data$change.val
    origin.data <- mapply(rep, as.list(timespines.data$node.origin+1), lapply(change.data, length))

    ## Function for one single plot
    plot.distribution <- function(group.data, histogram, scale.density, ...) {
        group_data <- as.numeric(group.data)

        ## Get the histogram data
        histogram_data <- hist(group_data, plot = FALSE)
        density_data <- density(group_data, na.rm = TRUE)

        if(scale.density) {
            ## Scaling the frequency/density
            histogram_data$density <- histogram_data$density/max(histogram_data$density)
            density_data$y <- density_data$y/max(density_data$y)
        }

        if(histogram) {
            ## Plot the histogram
            plot(histogram_data, border = "grey", yaxt = "n", ...)
            # plot(histogram_data, border = "grey", yaxt = "n") ; warning("DEBUG")
            ## Ticks on the y axis
            ticks <- axTicks(2)
            ## Labels on the y axis
            if(scale.density) {
                ## Scale the tick labels
                ticks_labels <- ticks/max(histogram_data$counts)

                ## Get the rounding parameters
                rounding <- 2
                if(max(ticks_labels) > 1){
                    rounding <- 1
                }
                if(max(ticks_labels) > 10){
                    rounding <- 0
                }

                ## Round the tip labels
                ticks_labels <- round(ticks_labels, digit = rounding)
            } else {
                ticks_labels <- ticks
            }
            ## Labels on the y axis
            axis(2, labels = ticks_labels, at = ticks)

            ## Scaling the density line
            density_data$y <- density_data$y * max(histogram_data$counts)

            ## Add the density
            lines(density_data)
        } else {
            ## Plot the density
            plot(density_data$x, density_data$y, type = "l", ...)
            # plot(density_data$x, density_data$y, type = "l") ; warning("DEBUG")
        }
    }

    plot.change.occurence <- function(change.data, group.data, origin.data, change, scale.density, col.change, ...) {
        change_data <- as.numeric(change.data)
        group_data <- as.numeric(group.data)

        ## Get the histogram data (for scaling)
        histogram_data <- hist(group_data, plot = FALSE)
        density_data <- density(group_data, na.rm = TRUE)

        if(scale.density) {
            ## Scaling the frequency/density
            histogram_data$density <- histogram_data$density/max(histogram_data$density)
            density_data$y <- density_data$y/max(density_data$y)
        }

        if(any(change %in% "hist")) {
            ## Getting the histogram data
            histogram_change <- hist(change_data, plot = FALSE)
            density_change <- density(change_data, na.rm = TRUE)
            ## Plotting the histogram results
            plot(histogram_change, col = "lightgrey", border = "darkgrey", add = TRUE, ...)
            # lines(density_change, lty = 2)

        } 

        if(any(change %in% "lines")) {
            ## Scaling the density line
            density_data$y <- density_data$y * max(histogram_data$counts)

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

            ## Add segments below the density line
            for(one_change in 1:length(change_data)) {
                x_value <- narrow.down.x(change_data[one_change], density_data$x)
                segments(x0 = density_data$x[x_value], y0 = 0, y1 = density_data$y[x_value], lwd = 1, lty = 3, col = col.change[origin.data[one_change]])
            }

        }

    }
            

    if(class(group.data) == "list") {
        ## Pooling the data
        group.data <- unlist(group.data)
    }
    if(class(change.data) == "list") {
        ## Pooling the data
        change.data <- unlist(change.data)
    }
    if(class(origin.data) == "list") {
        ## Pooling the data
        origin.data <- unlist(origin.data)
    }

    ## Plot the overall distribution
    plot.distribution(group.data, histogram = histogram, scale.density = scale.density, ...)

    ## Add the changes
    plot.change.occurence(change.data, group.data, origin.data, change = change, scale.density = scale.density, col.change = col.change, ...)
    if(legend) {
        legend("topright", legend = c("gain", "loss"), col = col.change, lty = 3, lwd = 2, bty = "n")
    }

}


#' @title Topology change
#'
#' @description Detects state changes based on a group topology
#'
#' @param group A \code{tree} that is the group.
#' @param data the dataset or the names of the species to check
#' @param state the state to evaluate
#' @param data.out the column in the dataset containing the data to output (set to 0 for rownames - default) or, if data is the names of species, the values to output
#' 
#' @examples
#' 
#' @return A \code{list} composed of the following elements:
#' \begin{itemize}
#'  \item \code{state.origin} the origin state (for the direction of the change)
#'  \item \code{changed.taxa} the taxa that have changed within the groups
#'  \item \code{group.taxa} all the taxa from the group
#'  \item \code{changed.val} the changed taxa values
#'  \item \code{group.val} the group values
#' \end{itemize}
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

topology.change <- function(group, data, state, data.out = 0) {

    ## Getting the members of the group in the data
    if(class(data) == "data.frame") {
        group_taxa <- match(group$tip.label, rownames(data))
    } else {
        group_taxa <- match(group$tip.label, data)
    }

    ## Getting the average group state
    state_origin <- dispRity::mode.val(state[group_taxa])

    ## Getting the taxa that have not the state_origin
    changed_taxa <- which(state[group_taxa] != state_origin)

    if(length(changed_taxa) == 0) {
        changed_taxa <- NULL
    }

    ## Getting the values out
    if(data.out == 0 && class(data) == "data.frame") {
        if(class(data) == "data.frame"){
            data_out <- rownames(data)[group_taxa]
        } else {
            data_out <- data[group_taxa]
        }
    } else {
        if(class(data) == "data.frame"){
            data_out <- data[group_taxa, data.out]
        } else {
            data_out <- data.out[group_taxa]
        }
    }

    ## Getting the changed and group values
    changed_val <- data_out[changed_taxa]
    group_val <- data_out

    return(list( "state.origin" = state_origin, "changed.taxa" = changed_taxa, "group.taxa" = group_taxa, "changed.val" = changed_val, "group.val" = group_val))
}
