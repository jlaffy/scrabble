
clean_by_similarity = function(clusters,
                               genes,
                               cutoff = 0.8,
                               order.method = 'combined',
                               ties.method = 'random',
                               first = 'sig1',
                               highest = TRUE,
                               decreasing = TRUE,
                               sortBySignificance = TRUE,
                               return.index = FALSE) {

    Order = order_by_significance(L = genes,
                                  method = order.method,
                                  ties.method = ties.method,
                                  first = first,
                                  highest = highest,
                                  decreasing = decreasing,
                                  return.index = TRUE)

    filteredClusters = jaccard_filter(clusters[Order],
                                      cutoff = cutoff,
                                      return.index = FALSE)

    if (return.index) {
        return(which(names(clusters) %in% names(filteredClusters)))
    }

    if (sortBySignificance) {
        return(filteredClusters)
    }

    clusters[names(clusters) %in% names(filteredClusters)]
}
