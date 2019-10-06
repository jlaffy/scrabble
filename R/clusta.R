#!/usr/bin/env Rscript

#' @title clusta
#' @description All things hierarchical clustering
#' @param mat matrix. Default: NULL
#' @param cr correlation matrix. If provided, cr will not be computed with mat. Default: FALSE
#' @param hc hclust object. If provided, hc will not be computed with cr. Default: FALSE
#' @param ord ordered character vector, retrieved from hc. Default: FALSE
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @return The function proceeds through the following steps: (1) compute correlation matrix, (2) compute hierarchical clustering object, (3) retrieve ordered character vector. It will start at the point for which an object is provided. i.e. if cr is provided, then step (1) is skipped. It will either return a list with all computed objects \code{list(cr = cr, hc = hc, ord = ord)} or stop at the point at which an argument is set to TRUE. i.e. if cr = T, step (3) is skipped and
#' only the correlation matrix is returned. Thus, users can provide an expression matrix and ask for the ordered columns' character vector, or provide a correlation matrix and ask for the corresponding hierarchical clustering object and so on.
#' @seealso 
#'  \code{\link[stats]{cor}},\code{\link[stats]{hclust}},\code{\link[stats]{dist}}
#' @rdname clusta
#' @export 
#' @importFrom stats cor hclust dist as.dist
clusta <- function(m = NULL,
                   cr = FALSE,
		   sim = FALSE,
                   hc = FALSE,
                   ord = FALSE,
                   hc.method = 'average', 
                   cor.method = 'pearson',
                   compute.dist = T,
                   dist.method = 'euclidean',
                   ord.labels = T) {

  # CORRELATION MATRIX
  # run?
    
    objects_to_compute = list(cr, hc, ord)
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
        cr <- stats::cor(mat, method = cor.method)
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

    # return everything
    list(cr = cr, hc = hc, ord = ord)

}
