#' @title Center a matrix column-wise 
#' @description Center a matrix column-wise 
#' @param m a matrix or Matrix
#' @param by either "mean", "median" or a numeric vector of length equal to the number of columns of ‘m’. Default: "mean"
#' @return column-centered matrix
#' @rdname colCenter
#' @export 
colCenter = function(m, by = 'mean') {
    m = as.matrix(m)
    if (by == 'mean')  by = T
    else if (by == 'median') by = matrixStats::colMedians(m)
    else stopifnot(is.numeric(by) & length(by) == ncol(m))
    scale(m, center = by, scale = F)
}

#' @title Center a matrix row-wise 
#' @description Center a matrix row-wise 
#' @param m a matrix or Matrix
#' @param by either "mean", "median" or a numeric vector of length equal to the number of rows of ‘m’. Default: "mean"
#' @return row-centered matrix
#' @rdname rowCenter
#' @export 
rowCenter = function(m, by = 'mean') {
    m = as.matrix(m)
    if (by == 'mean')  by = T
    else if (by == 'median') by = matrixStats::rowMedians(m)
    else stopifnot(is.numeric(by) & length(by) == nrow(m))
    t(scale(t(m), center = by, scale = F))
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
