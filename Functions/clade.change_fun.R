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
#' @param group.data A vector or a list
#' @param change.data A vector or a list
#' @param histogram logical, whether to plot the histogram in the background (TRUE)
#' @param scale.density logical, whether to scale the density (TRUE)
#' @param change.hist logical, whether to display the changes a histogram (TRUE)
#' @param ... Arguments to be passed to plot()
#'
#' @examples
#' 
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export


plot.change <- function(group.data, change.data, histogram = TRUE, scale.density = TRUE, change.hist = TRUE, ...) {

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

    plot.change.occurence <- function(change.data, group.data, change.hist, scale.density, ...) {
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

        if(change.hist) {
            ## Getting the histogram data
            histogram_change <- hist(change_data, plot = FALSE)
            density_change <- density(change_data, na.rm = TRUE)
            ## Plotting the histogram results
            plot(histogram_change, col = "lightgrey", border = "darkgrey", add = TRUE, ...)
            lines(density_change, lty = 2)

        } else {
            ## Scaling the density line
            density_data$y <- density_data$y * max(histogram_data$counts)

            ## Add segments below the density line
            for(one_change in 1:length(change_data)) {
                x_value <- narrow.down.x(change_data[one_change], density_data$x)
                segments(x0 = density_data$x[x_value], y0 = 0, y1 = density_data$y[x_value], lwd = 1)
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

    ## Plot the overall distribution
    plot.distribution(group.data, histogram = histogram, scale.density = scale.density, ...)

    ## Add the changes
    plot.change.occurence(change.data, group.data, change.hist = change.hist, scale.density = scale.density, ...)
}


#' @title Topology change
#'
#' @description Detects state changes based on a group topology
#'
#' @param group A \code{tree} that is the group.
#' @param data the dataset
#' @param state the state to evaluate
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

topology.change <- function(group, data, state, data.out = 0) {

    ## Getting the members of the group in the data
    group_taxa <- match(group$tip.label, rownames(data))

    ## Getting the average group state
    state_origin <- dispRity::mode.val(state[group_taxa])

    ## Getting the taxa that have not the state_origin
    changed_taxa <- which(state[group_taxa] != state_origin)

    if(length(changed_taxa) == 0) {
        changed_taxa <- NULL
    }

    ## Getting the values out
    if(data.out == 0) {
        data_out <- rownames(data)[group_taxa]
    } else {
        data_out <- data[group_taxa, data.out]
    }

    ## Getting the changed and group values
    changed_val <- data_out[changed_taxa]
    group_val <- data_out

    return(list( "state.origin" = state_origin, "changed.taxa" = changed_taxa, "group.taxa" = group_taxa, "changed.val" = changed_val, "group.val" = group_val))
}