
#' @title Remove Clusters that Contain Too Few or Too Many Members
#' @description This function removes clusters that contain too few or too many members. The allowed sizes of clusters can be specified relative to the total number of members with min.rel and max.rel, or as absolute values with min.size and max.size. If one of the minimum or maximum size cutoffs are not desired, the min.size/max.size argument should be set to FALSE. By default, the min.size and max.size arguments are set to TRUE, which defines the cluster sizes allowed according to min.rel and max.rel and the total number of members across all clusters.
#' @param clusters List of cluster memberships (character vectors)
#' @param min.size a boolean value or numeric value indicating whether there should be a minimum number of clusters or what the minimum number of clusters should be, respectively. If the former, then the minimum number of clusters will be calculated relative to the total number of clusters with min.rel.
#' @param max.size a boolean value or numeric value indicating whether there should be a maximum number of clusters or what the maximum number of clusters should be, respectively. If the former, then the maximum number of clusters will be calculated relative to the total number of clusters with max.rel.
#' @param min.rel minimum relative size of a cluster. Default: 0.01 (ie. 0.01 of total number of variables)
#' @param max.rel maximum relative size of a cluster. Default: 0.8 (ie. 0.8 of total number of variables)
#' @param min.abs absolute minimum size. If min.rel < min.abs, min.abs will replace min.rel. Default: 5
#' @return a list of filtered clusters according to their being within min.rel and max.rel cutoffs.
#' @rdname clean_by_size
#' @export 
clean_by_size = function(clusters,
                         min.size = T,
                         max.size = T,
                         min.rel = 0.01,
                         max.rel = 0.8,
                         min.abs = 5) {

    n = length(unlist(clusters) %>% unique)

    if (isTRUE(min.size)) {
        min.size = round(min.rel * n, 1)
        if (min.size < min.abs) {
            min.size = min.abs
        }
    }

    if (isTRUE(max.size)) {
        max.size = round(max.rel * n, 1)
    }

    if (!is.numeric(min.size) & !is.numeric(max.size)) {
        return(clusters)
    }
    
    else if (!is.numeric(max.size)) {
        return(clusters[lengths(clusters) >= min.size])
    }

    else if (!is.numeric(min.size)) {
        return(clusters[lengths(clusters) <= max.size])
    }
    
    clusters[lengths(clusters) >= min.size & lengths(clusters) <= max.size]
    
}
