## Analysis wrapper
#@param body.val, a vector of body lengths or mass (e.g. spine_data$spine_data[,3])
#@param armour.val, a vector of armour scores (of same length as body.val, e.g. spine_data$armour[,1])
#@param tree
#@param method, which method to use: "topol" or "brlen" for topology or topology + branch length
#@param taxa.names
#@param do.log, logical, whether to log the data or not (default = TRUE)
#@param do.scale, logical, whether to scale the data or not (default = TRUE)

# stop("DEBUG run.timespines")
# body.val <- body_size_values
# armour.val <- armour_values
# tree
# method = "topol"
# taxa.names = rownames(spine_data$spine_data)

run.timespines <- function(body.val, armour.val, tree, method = "topol", taxa.names, do.log = TRUE, do.scale = TRUE) {
    
    ## The armour values
    armour_character <- paste(armour.val, collapse = "")

    ## Traversal of the tree to detect changes.
    traversal_results <- Inapp::apply.reconstruction(tree, armour_character, passes = 1, method = "Fitch", match.tip.char = TRUE)

    ## Get the subtrees with a state change
    subtrees <- sapply(traversal_results$changes, function(X, tree) return(extract.clade(tree, node = X)), tree = tree, simplify = FALSE)

    ## Get the state before change
    origin <- sapply(traversal_results$changes, function(X, tree, traversal_results) return(traversal_results$Dp1[[tree$edge[,1][which(tree$edge[,2] == X)]]]), tree = tree, traversal_results = traversal_results, simplify = FALSE)

    ## Get the changes per clades
    armour_changes <- lapply(subtrees, topology.change, data = taxa.names, state = armour.val, data.out = body.val)

    ## Extract the changes values
    normal_values <- extract.change(armour_changes, what = "group.val")

    if(method == "topol") {
        change_values <- extract.change(armour_changes, what = "changed.val")
    }

    if(method == "brlen") {
        ## Performing the ancestral states reconstruction
        ace_body_values <- ace(body.val[match(tree$tip.label, taxa.names)], tree, method = "pic")

        ## Extract the changes for each group
        change_values <- ace_body_values$ace[traversal_results$changes - Ntip(tree)]
    }

    ## Log
    if(do.log) {
        normal_values <- lapply(normal_values, log)
        change_values <- lapply(change_values, log)
    }

    ##  Scale
    if(do.scale){
        ## get the maximum of each group
        max_normal <- lapply(normal_values, max, na.rm = TRUE)

        ## Normalising the body sizes relative to their group maximum
        scale.fun <- function(X, Y) scale(X, center = FALSE, scale = Y)
        normal_values <- mapply(scale.fun, normal_values, max_normal, SIMPLIFY = FALSE)

        if(method == "topol") {
            change_values <- mapply(scale.fun, change_values, max_normal, SIMPLIFY = FALSE)
        } else {
            change_values <- mapply(scale.fun, as.list(change_values), max_normal, SIMPLIFY = FALSE)
        }
    }

    changed_taxa <-  extract.change(armour_changes, what = "changed.taxa")

    return(list("normal.val" = normal_values, "change.val" = change_values, "change.taxa" = changed_taxa, "node.changes" = traversal_results$changes, "node.origin" = unlist(origin)))

}



## Analysis wrapper
#@param timespines, The timespines data output from \code{run.timespines}
#@param what, either 0 for gain or 1 for loss.
#@param statistic, the statistic to apply (default = median)
#@param replicates (default = 500)
#@param rarefaction (default = TRUE)=

run.bootstraps <- function(timespines.data, what, statistic = median, replicates = 100, rarefaction = TRUE) {

    ## Running the test for the topology

    ## Getting the origin values
    origin.data <- unlist(mapply(rep, as.list(timespines.data$node.origin), lapply(timespines.data$change.val, length)))

    ## Adding the estimate ancestral values to the pooled distribution
    distribution <- c(unlist(timespines.data$normal.val), unlist(timespines.data$change.val)[which(origin.data == what)])

    ## The IDs to test are the last one in the distribution
    subset <- seq(from = length(unlist(timespines.data$normal.val)) + 1, to = length(distribution))

    ## Applying the random test for the topology
    test_results <- landvR::rarefy.stat(matrix(distribution[subset]), stat.fun  = statistic, replicates = replicates, rarefaction = NULL)
    return(test_results)
}
