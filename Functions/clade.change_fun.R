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

