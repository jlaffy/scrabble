
#' @title fold_changes
#' @description Compute (log2) Fold-Changes between Groups
#' @param m matrix 1
#' @param m2 matrix 2. If null, m2 = m[, !group]. Defaut: NULL
#' @param group if m2 not supplied, m = m[, group]; m2 = m[, !group]. Default: NULL
#' @param is.log values are in log2 (used in fold_changes), Default: T
#' @return fold change values for rows in m
#' @details DETAILS
#' @rdname fold_changes
#' @export 
fold_changes = function(m,
                        m2 = NULL,
                        group = NULL,
                        is.log = T) {

    if (is.null(m2)) {
        stopifnot(!is.null(group) & all(group %in% colnames(m)))
        m2 = m[, setdiff(colnames(m), group), drop = F]
        m = m[, group, drop = F]
    }
    if (is.log) {
        return(rowMeans(m) - rowMeans(m2))
    }
    
    rowMeans(m)/rowMeans(m2)
}

