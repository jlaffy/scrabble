
#' @title Binary "in or out" Matrix
#' @description Computes the membership of strings in a series of character vectors, provided either in list or matrix format (vector == column). 
#' @param X a list or matrix of character vectors or columns
#' @param order a boolean value indicating whether the resulting membership matrix should have rows and columns reordered. Default: F
#' @param col.order a boolean value indicating whether the resulting membership matrix should have columns reordered. Default: F
#' @param row.order a boolean value indicating whether the resulting membership matrix should have rows reordered. Default: F
#' @param dist.method a character string indicating the distance method to be used for the hierarchichal ordering. e.g. 'manhattan', 'euclidean'. Default: 'manhattan'
#' @return a matrix of 0s and 1s for each of the unique strings in X in each of the vectors or columns in X.
#' @rdname membership
#' @export 
membership = function(X,
                      universe = NULL,
                      order = F,
                      col.order = order,
                      row.order = order,
                      dist.method = 'manhattan') {

    if (!is.null(dim(X))) {
        X = as.list(as.data.frame(X, stringsAsFactors = F))
    }

    if (is.null(universe)) {
        universe = unique(unlist(X, use.names = F))
    }

    mat = sapply(X, function(Group) sapply(universe, function(Var) as.numeric(Var %in% Group)))

    mat = scrabble::reorder(mat,
                            corr = FALSE,
                            dist.method = dist.method,
                            col = col.order,
                            row = row.order)
    mat
}
