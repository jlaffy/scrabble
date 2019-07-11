#!/usr/bin/env Rscript

#' @title data matrix --> ordered correlation matrix in one word
#' @description Calls the scrabble function clsuta() to generate a correlation matrix with the parameters specified and subsequently order the columns and rows by hierarchical clustering. 
#' @param mat matrix. Default: NULL
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @return an ordered correlation matrix with the same number of rows and columns as there were columns in the original matrix.
#' @seealso 
#'  \code{\link[scrabble]{clusta}}
#' @rdname creorder
#' @export 
#' @importFrom scrabble clusta
creorder = function(mat = NULL,
                    hc.method = 'average', 
                    cor.method = 'pearson',
                    compute.dist = T,
                    dist.method = 'euclidean',
                    ord.labels = T) {

    obj = scrabble::clusta(mat = mat,
                           hc.method = hc.method,
                           cor.method = cor.method,
                           compute.dist = compute.dist,
                           dist.method = dist.method,
                           ord.labels = ord.labels)

    obj$CR[obj$ORD, obj$ORD]
}
