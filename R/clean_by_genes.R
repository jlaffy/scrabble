#' @title clean_by_genes
#' @description Filter for Statistically Significant Clusters
#' @param L List of clusters' differentially expressed genes
#' @param nsig1 cutoff for number of differentially expressed genes. Default: 30
#' @param nsig2 cutoff for number of most differentially expressed genes. Default: 5
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
clean_by_genes = function(L, nsig1 = 30, nsig2 = 5) {
    # L is list of differentially expressed gene values from different clusters
    L[lengths(L) >= nsig1 & lengths(uniquely(L, highest = T)) >= nsig2]
}


