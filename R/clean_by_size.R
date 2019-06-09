
#' @title clean_by_size
#' @description Filtering for Reasonably Sized Clusters
#' @param clusters List of cluster memberships (character vectors)
#' @param min.rel minimum relative size of a cluster. Default: 0.01 (ie. 0.01 of total number of variables)
#' @param max.rel maximum relative size of a cluster. Default: 0.8 (ie. 0.8 of total number of variables)
#' @param min.abs absolute minimum size. If min.rel < min.abs, min.abs will replace min.rel. Default: 5
#' @return a list of filtered clusters according to their being within min.rel and max.rel cutoffs.
#' @details DETAILS
#' @rdname clean_by_size
#' @export 
clean_by_size = function(clusters,
                         min.rel = 0.01,
                         max.rel = 0.8,
                         min.abs = 5) {

    n = length(unlist(clusters) %>% unique)
    min.size = round(min.rel * n, 1)
    max.size = round(max.rel * n, 1)
    if (min.size < min.abs) {
        min.size = min.abs
    }
    clusters[lengths(clusters) >= min.size & lengths(clusters) <= max.size]
}


