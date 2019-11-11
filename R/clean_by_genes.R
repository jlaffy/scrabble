#' @title Filter for Statistically Significant Clusters
#' @description Filter for Statistically Significant Clusters
#' @param L List of clusters' differentially expressed genes
#' @param nsig1 cutoff for number of differentially expressed genes. Set to FALSE if not desired. Default: 30
#' @param nsig2 cutoff for number of most differentially expressed genes. Set to FALSE if not desired. Default: 5
#' @param return.index boolean value indicating whether a vector of indexes to keep should be returned instead of the filtered list. Default: FALSE
#' @return a filtered list of statistically significant clusters. ie. those passing nsig1 and nsig2 cutoffs.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clean_by_genes
#' @export 
clean_by_genes = function(L,
                          nsig1 = 30,
                          nsig2 = 5,
                          return.index = FALSE) {

    sig1 = lengths(L)
    sig2 = lengths(uniquely(L, highest = T))

    if (is.numeric(nsig1) & is.numeric(nsig2)) {
        ind = which(sig1 >= nsig1 & sig2 >= nsig2)
    }

    else if (is.numeric(nsig1)) {
        ind = which(sig1 >= nsig1)
    }

    else if (is.numeric(nsig2)) {
        ind = which(sig2 >= nsig2)
    }

    else stop('At least one of nsig1 or nsig2 should be numeric.')

    if (return.index) {
        return(ind)
    }

    L[ind]
}


