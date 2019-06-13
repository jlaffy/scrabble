
#' @title Ordered List Filter by Jaccard Similarity
#' @description Computes the jaccard similarities of all pairs of vectors, and, for any pair with too-high similarity, removes the vector that is placed lower in the list.
#' @param L List of character vectors
#' @param cutoff numeric value between 0 and 1. Default: 0.8
#' @param return.index boolean value indicating whether a vector of indexes to keep should be returned instead of the filtered list. Default: FALSE
#' @return Filtered list of character vectors whose pairwise Jaccard similarities are all smaller than or equal to the cutoff specified. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname jaccard_filter
#' @export 
jaccard_filter = function(L, cutoff = 0.8, return.index = FALSE) {
    
    stopifnot(cutoff >= 0 and <= 1)

    Is = 1:(length(L) - 1)
    Js = sapply(2:length(L), function(i) i:length(L))

    .jaccard_too_similar = function(x, y, L, cutoff) {
        x + (which(jaccard(L[x], L[y]) > cutoff))
    }
    
    removing = Map(.jaccard_too_similar,
                   x = Is,
                   y = Js,
                   MoreArgs = list(L = L, cutoff = cutoff))

    removing = unique(unlist(removing))

    if (return.index) {
        return(setdiff(1:length(L), removing))
    }

    L[-1 * removing]
}
