
.morder.cormat = function(m, col = T, row = T) {
    ord = HC(cr = m, ord = T)
    if (col) m = m[, ord]
    if (row) m = m[ord, ]
    m
}

#' @title Reorder matrix columns and rows
#' @description Reorder matrix columns and rows by hierarchical clustering of pairwise pearson correlation values
#' @param m a matrix
#' @param col logical indicating whether to reorder columns. Default: T
#' @param row logical indicating whether to reorder rows. Default: T
#' @return ordered matrix
#' @rdname morder
#' @export 
morder = function(m, col = T, row = T) {
    if (is.simil.matrix(m)) {
        return(.morder.cormat(m, col = col, row = row))
    }
    if (col) m = m[, HC(m = m, ord = T)]
    if (row) m = m[HC(m = t(m), ord = T), ]
    m
}
