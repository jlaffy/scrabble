#!/usr/bin/env Rscript

#' @title Reorder a Matrix 
#' @description Calls the scrabble function clusta() to order the columns and/or the rows of a matrix by hierarchical clustering.
#' @param mat matrix. Default: NULL
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @param both a boolean value indicating whether both rows and columns should be reordered. Default: F
#' @param col a boolean value indicating whether columns should be reordered. Default: T
#' @param row a boolean value indicating whether rows should be reordered. Default: F
#' @return the matrix, with rows and/or columns reordered. 
#' @seealso 
#'  \code{\link[scrabble]{clusta}}
#' @rdname reorder
#' @export 
reorder = function(mat = NULL,
                   sim.mat = NULL,
                   corr = TRUE,
                   hc.method = 'average', 
                   cor.method = 'pearson',
                   compute.dist = T,
                   dist.method = 'euclidean',
                   both = T, 
                   row = both,
                   col = both,
                   ord.labels = T) {

    cr = FALSE

    if (!corr) {
        Col = row
        row = col
        col = Col
    }

    if (col) {
        if (!corr) cr = mat
        ord = scrabble::clusta(mat = mat,
                               cr = cr,
                               hc.method = hc.method,
                               cor.method = cor.method,
                               compute.dist = compute.dist,
                               dist.method = dist.method,
                               ord.labels = ord.labels,
                               ord = T)
        if (!corr) mat = mat[ord, ]
        else mat = mat[, ord]
    }

    if (row) {
        if (!corr) cr = t(mat)
        ord = scrabble::clusta(mat = mat,
                               cr = cr,
                               hc.method = hc.method,
                               cor.method = cor.method,
                               compute.dist = compute.dist,
                               dist.method = dist.method,
                               ord.labels = ord.labels,
                               ord = T)
        if (!corr) mat = mat[, ord]
        else mat = mat[ord, ]
    }

    mat
}
