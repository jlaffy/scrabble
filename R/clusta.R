#!/usr/bin/env Rscript

#' @title clusta
#' @description All things hierarchical clustering
#' @param mat matrix. Default: NULL
#' @param CR correlation matrix. If provided, CR will not be computed with mat. Default: FALSE
#' @param HC hclust object. If provided, HC will not be computed with CR. Default: FALSE
#' @param ORD ordered character vector, retrieved from HC. Default: FALSE
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @return The function proceeds through the following steps: (1) compute correlation matrix, (2) compute hierarchical clustering object, (3) retrieve ordered character vector. It will start at the point for which an object is provided. i.e. if CR is provided, then step (1) is skipped. It will either return a list with all computed objects \code{list(CR = cr, HC = hc, ORD = ord)} or stop at the point at which an argument is set to TRUE. i.e. if CR = T, step (3) is skipped and
#' only the correlation matrix is returned. Thus, users can provide an expression matrix and ask for the ordered columns' character vector, or provide a correlation matrix and ask for the corresponding hierarchical clustering object and so on.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stats]{cor}},\code{\link[stats]{hclust}},\code{\link[stats]{dist}}
#' @rdname clusta
#' @export 
#' @importFrom stats cor hclust dist as.dist
clusta <- function(mat = NULL,
                   CR = FALSE,
                   HC = FALSE,
                   ORD = FALSE,
                   hc.method = 'average', 
                   cor.method = 'pearson',
                   compute.dist = T,
                   dist.method = 'euclidean',
                   ord.labels = T) {

  # CORRELATION MATRIX
  # run?
    
    if (is.null(mat) & sum(sapply(list(CR, HC), is.logical)) == 2) {
        stop('One of mat, CR or HC must be provided')
    }

    if (is.logical(CR)) {
      cr <- stats::cor(mat, method = cor.method)
    }
    # should the function stop here?
    if (isTRUE(CR)) {
      return(cr)
    }
    # or was the input provided?
    else if (!is.null(dim(CR))) {
      cr <- CR
    }


    # HIERARCHICAL CLUSTER OBJECT
    # run?
    if (is.logical(HC)) {
        if (isTRUE(compute.dist)) {
            hc <- stats::hclust(stats::dist(1 - cr, method = dist.method), method = hc.method)
        } else {
            hc <- stats::hclust(stats::as.dist(1 - cr), method = hc.method)
        }
    }
    # should the function stop here?
    if (isTRUE(HC)) {
      return(hc)
    }
    # or was the input provided?
    else if (class(HC) == 'hclust') {
      hc <- HC
    }


    # HIERARCHICAL CLUSTER ORDER
    # run?
    if (is.logical(ORD) & isTRUE(ord.labels)) {
      ord <- hc$labels[hc$order]
    }

    else if (is.logical(ORD)) {
        ord <- hc$order
    }

    # should the function stop here?
    if (isTRUE(ORD)) {
      return(ord)
    }

    # return everything
    list(CR = cr, HC = hc, ORD = ord)

}
