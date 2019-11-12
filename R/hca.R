#' @title Hierarchical Clustering Analysis
#' @description Hierarchical clustering analysis. The function proceeds through the following steps: 1. Compute a correlation matrix; 2. Compute a distance matrix; 3. Compute hclust (hierarchical clustering) object; 4. Retrieve the clustered column order (included for practical usability purposes only); 5. Extract the clusters from the analysis (from the hclust object). 
#' Because each of these intermediary objects are useful, the function allows you to leave at any point to return the object of interest. Do this by setting the corresponding object argument to TRUE. The function also allows you to enter at any point to skip steps that you have already computed elsewhere. To do this provide the starting object to the corresponding argument. You can also choose to return all intermediary steps by setting return.steps = T.
#' @param m matrix. Default: NULL
#' @param cr correlation matrix. If provided, cr will not be computed with m. Default: FALSE
#' @param d dist object. If provided, d will not be computed from cr. Default: FALSE
#' @param hc hclust object. If provided, hc will not be computed with cr. Default: FALSE
#' @param ord ordered character vector, retrieved from hc. Default: FALSE
#' @param clusters list of clusters (character vectors), retrieved from hc. Default: FALSE
#' @param return.steps logical indicating whether to return intermediary steps. Default: FALSE
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: ''euclidean'
#' @param ord.labels if FALSE, will return ordered indices rather than character vector. Default: T
#' @return object or list of objects. If the latter, a full list contains m (input matrix to be clustered), cr (correlation matrix), d (distance matrix), hc (hclust object), ord (char. vector), clusters (list of char. vectors).
#' @seealso 
#'  \code{\link[stats]{cor}},\code{\link[stats]{hclust}},\code{\link[stats]{dist}}
#' @rdname hca
#' @export 
#' @importFrom stats cor hclust dist as.dist
hca = function(m = NULL,
               cr = FALSE,
               d = FALSE,
               hc = FALSE,
               ord = FALSE,
		       clusters = FALSE,
               return.steps = FALSE,
               hc.method = 'average', 
               cor.method = 'pearson',
               compute.dist = T,
               dist.method = 'euclidean',
               ord.labels = T,
  		       h = NULL,
		       k = NULL,
		       min.cluster.size = 5,
		       max.cluster.size = 0.8) {

  # CORRELATION MATRIX
  # run?
    
    List = c()
    objects_to_compute = list(cr, d, hc, ord, clusters)
    start_computation = 0
    end_computation = 5
    custom_start = sapply(objects_to_compute, function(obj) !is.logical(obj))
    custom_end = sapply(objects_to_compute, isTRUE)

    if (any(custom_start)) {
        start_computation = max(which(custom_start))
    }

    if (any(custom_end)) {
        end_computation = max(which(custom_end))
    }

    if (start_computation == 0) {
        List = c(List, list(m = m))
        cr = stats::cor(m, method = cor.method)
        cr[is.na(cr)] <- 0
        start_computation = start_computation + 1
    }

    List = c(List, list(cr = cr))
    
    if (end_computation == 1) {
        if (return.steps) return(List)
        return(cr)
    }

    if (start_computation == 1) {
        if (compute.dist) d = stats::dist(1 - cr, method = dist.method)
        else d = stats::as.dist(1 - cr)
        start_computation = start_computation + 1
    }

    List = c(List, list(d = d))
    
    if (end_computation == 2) {
        if (return.steps) return(List)
        return(d)
    }

    if (start_computation == 2) {
        hc = stats::hclust(d, method = hc.method)
        start_computation = start_computation + 1
    }

    List = c(List, list(hc = hc))

    if (end_computation == 3) {
        if (return.steps) return(List)
        else return(hc)
    }

    if (start_computation == 3) {
        if (!ord.labels) ord = hc$order
        else ord = hc$labels[hc$order]
        start_computation = start_computation + 1
    }

    List = c(List, list(ord = ord))

    if (end_computation == 4) {
        if (return.steps) return(List)
        else return(ord)
    }

    if (start_computation == 4) {
	    clusters = .extractClusters(hc = hc,
				                    h = h,
				                    k = k,
				                    min.cluster.size = min.cluster.size,
				                    max.cluster.size = max.cluster.size)
        start_computation = start_computation + 1
    }

    List = c(List, list(clusters = clusters))

    if (end_computation == 5) {
        # returns everything
        if (return.steps) return(List)
        return(clusters)
    }

    List
}
