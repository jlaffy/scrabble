#!/usr/bin/env Rscript

.extractClusters = function(hc = NULL,
                     	    k = NULL,
                            h = NULL,
                            min.cluster.size = 0,
                            max.cluster.size = 1) {

    if (is.null(h) & is.null(k)) h = hc$height # all heights
    Clusters = stats::cutree(tree = hc, h = h, k = k)
    colnames(Clusters) <- paste0(round(as.numeric(colnames(Clusters)), 4), "_") # preparing clusterNames
    clusterNames = names(unlist(apply(Clusters, 2, function(col) 1:length(table(col))))) # new clusterNames
    labels = rownames(Clusters)
    Clusters = as.list(as.data.frame(Clusters))
    Clusters = sapply(Clusters, function(ID) split(labels, ID), simplify =F)
    Clusters = unlist(Clusters, recursive = F, use.names = F)
    Clusters = stats::setNames(Clusters, clusterNames)
    ncells = length(unique(unlist(Clusters)))
    if (min.cluster.size >= 0 & min.cluster.size <= 1) {
	    min.cluster.size = min.cluster.size * ncells
    }
    if (max.cluster.size >= 0 & max.cluster.size <= 1) {
	    max.cluster.size = max.cluster.size * ncells
    }
    lens = lengths(Clusters)
    Clusters = Clusters[lens >= min.cluster.size & lens <= max.cluster.size]
    Clusters
}

#' @title hCluster
#' @description Hierarchical clustering
#' @param m matrix. Default: NULL
#' @param cr correlation matrix. If provided, cr will not be computed with m. Default: FALSE
#' @param hc hclust object. If provided, hc will not be computed with cr. Default: FALSE
#' @param ord ordered character vector, retrieved from hc. Default: FALSE
#' @param clusters list of clusters (character vectors), retrieved from hc. Default: FALSE
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @return The function proceeds through the following steps: (1) compute correlation matrix, (2) compute hierarchical clustering object, (3) retrieve ordered character vector. It will start at the point for which an object is provided. i.e. if cr is provided, then step (1) is skipped. It will either return a list with all computed objects \code{list(cr = cr, hc = hc, ord = ord)} or stop at the point at which an argument is set to TRUE. i.e. if cr = T, step (3) is skipped and
#' only the correlation matrix is returned. Thus, users can provide an expression matrix and ask for the ordered columns' character vector, or provide a correlation matrix and ask for the corresponding hierarchical clustering object and so on.
#' @seealso 
#'  \code{\link[stats]{cor}},\code{\link[stats]{hclust}},\code{\link[stats]{dist}}
#' @rdname hCluster
#' @export 
#' @importFrom stats cor hclust dist as.dist
hCluster = function(m = NULL,
                    cr = FALSE,
                    hc = FALSE,
                    ord = FALSE,
		            clusters = FALSE,
                    hc.method = 'average', 
                    cor.method = 'pearson',
                    compute.dist = T,
                    dist.method = 'euclidean',
                    ord.labels = T,
  		            h = NULL,
		            k = NULL,
		            min.cluster.size = 5,
		            max.cluster.size = 0.8) {

  # CORRELATION MATRIX
  # run?
    
    objects_to_compute = list(cr, hc, ord, clusters)
    start_computation = 0
    end_computation = 4
    custom_start = sapply(objects_to_compute, function(obj) !is.logical(obj))
    custom_end = sapply(objects_to_compute, isTRUE)

    if (any(custom_start)) {
        start_computation = max(which(custom_start))
    }

    if (any(custom_end)) {
        end_computation = max(which(custom_end))
    }

    if (start_computation == 0) {
        cr <- stats::cor(m, method = cor.method)
        start_computation = start_computation + 1
    }

    if (end_computation == 1) {
        return(cr)
    }

    if (start_computation == 1) {
        if (compute.dist) d = stats::dist(1 - cr, method = dist.method)
        else d = stats::as.dist(1 - cr)
        hc = stats::hclust(d, method = hc.method)
        start_computation = start_computation + 1
    }

    if (end_computation == 2) {
        return(hc)
    }

    if (start_computation == 2) {
        if (!ord.labels) ord = hc$order
        else ord = hc$labels[hc$order]
        start_computation = start_computation + 1
    }

    if (end_computation == 3) {
      return(ord)
    }

    if (start_computation == 3) {
	    clusters = .extractClusters(hc = hc,
				                    h = h,
				                    k = k,
				                    min.cluster.size = min.cluster.size,
				                    max.cluster.size = max.cluster.size)
        start_computation = start_computation + 1

    if (end_computation == 4) {
        return(clusters)
    }

    # return everything
    list(cr = cr, hc = hc, ord = ord, clusters = clusters)
}
