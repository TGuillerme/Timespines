## Analysis wrapper
#@param body.val, a vector of body lengths or mass (e.g. spine_data$spine_data[,3])
#@param armour.val, a vector of armour scores (of same length as body.val, e.g. spine_data$armour[,1])
#@param tree
#@param method, which method to use: "topol" or "brlen" for topology or topology + branch length
#@param taxa.names
#@param do.log, logical, whether to log the data or not (default = TRUE)
#@param do.scale, logical, whether to scale the data or not (default = TRUE)

run.timespines <- function(body.val, armour.val, tree, method = "topol", taxa.names, do.log = TRUE, do.scale = TRUE) {
    
    ## The armour values
    armour_character <- paste(armour.val, collapse = "")

    ## Traversal of the tree to detect changes.
    traversal_results <- Inapp::apply.reconstruction(tree, armour_character, passes = 1, method = "Fitch", match.tip.char = TRUE)

    ## Get the subtrees with a state change
    subtrees <- sapply(traversal_results$changes, function(X, tree) return(extract.clade(tree, node = X)), tree = tree, simplify = FALSE)

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

    ##  Scale
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

    return(list("normal.val" = normal_values, "change.val" = change_values, "change.taxa" = changed_taxa))

}
