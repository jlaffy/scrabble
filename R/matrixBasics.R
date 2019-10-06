#' @title Center a matrix column-wise 
#' @description Center a matrix column-wise 
#' @param m a matrix
#' @param method a character string. one of "mean" or "median" definining how to do the centering. Default: "mean"
#' @param centerBy a numeric vector of length equal to ncol(m) of values to be subtracted from each row.
#' @return a matrix centered by columns such that the statistic defined in method is equal to 0 for each column.
#' @rdname colCenter
#' @export 
colCenter = function(m, method = 'mean', centerBy = NULL) {
    if (!is.null(centerBy)) {
        stopifnot(length(centerBy) == ncol(m))
        center = centerBy
    }
    else if (method == 'median') center = apply(m, 2, stats::median)
    else if (method == 'mean') center = T
    else stop('method not recognised')
    scale(m, center = center, scale = F)
}


#' @title Center a matrix row-wise 
#' @description Center a matrix row-wise 
#' @param m a matrix
#' @param method a character string. one of "mean" or "median" definining how to do the centering. Default: "mean"
#' @param centerBy a numeric vector of length equal to nrow(m) of values to be subtracted from each row.
#' @return a matrix centered by rows such that the statistic defined in method is equal to 0 for each column.
#' @rdname rowCenter
#' @export 
rowCenter = function(m, method = 'mean', centerBy = NULL) {
    if (!is.null(centerBy)) {
        stopifnot(length(centerBy) == nrow(m))
        center = centerBy
    }
    else if (method == 'median') center = apply(m, 1, stats::median)
    else if (method == 'mean') center = T
    else stop('method not recognised')
    t(scale(t(m), center = center, scale = F))
}


#' @title <dim> for many matrices
#' @description Returns the result of dim for every matrix in a list
#' @param m a list of matrices (or a single matrix)
#' @return dim for each matrix provided.
#' @rdname dims
#' @export 
dims <- function(mats) {
    # if mats is a single matrix:
    if (!is.null(dim(mats))) {
        return(dim(mats))
    }
    # if mats is a list of matrices:
    sapply(mats, dim, simplify = T)
}


#' @title <ncol> for many matrices
#' @description Returns the result of ncol for every matrix in a list
#' @param m a list of matrices (or a single matrix)
#' @return ncol for each matrix provided.
#' @rdname ncols
#' @export 
ncols <- function(mats) {
    # if mats is a single matrix:
    if (!is.null(dim(mats))) {
        return(ncol(mats))
    }
    # if mats is a list of matrices:
    sapply(mats, ncol, simplify = T)
}


#' @title <nrow> for many matrices
#' @description Returns the result of nrow for every matrix in a list
#' @param m a list of matrices (or a single matrix)
#' @return nrow for each matrix provided.
#' @rdname nrows
#' @export 
nrows <- function(mats) {
    # if mats is a single matrix:
    if (!is.null(dim(mats))) {
        return(nrow(mats))
    }
    # if mats is a list of matrices:
    sapply(mats, nrow, simplify = T)
}


#' @title Range of the rowMeans of a matrix
#' @description Returns the range over the rowMeans of a matrix
#' @param mat a matrix
#' @return the range of the rowMeans of the matrix provided.
#' @rdname range_rowMeans
#' @export 
range_rowMeans <- function(mat) {
    range(rowMeans(mat))
}
