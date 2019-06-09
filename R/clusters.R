
#' @title clusters
#' @description Compute and Retrieve Clusters from Hierarchical Clustering at all Heights
#' @param m matrix. Default: NULL
#' @param hc hierarchical clustering (hclust) object. If provided, will be used instead of computed. Default: NULL
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: 'euclidean'
#' @param k number of clusters to return. Default: NULL
#' @param h tree height at which to retrieve clusters. Default: NULL
#' @param clean if TRUE, will return clusters that are within the sizes specified. Default: TRUE
#' @param min.rel minimum relative size of a cluster. Default: 0.01 (ie. 0.01 of total number of variables)
#' @param max.rel maximum relative size of a cluster. Default: 0.8 (ie. 0.8 of total number of variables)
#' @param min.abs absolute minimum size. If min.rel < min.abs, min.abs will replace min.rel. Default: 5
#' @return with default parameters, will compute hierarchical clustering object of the pariwise correlations of the matrix provided and retrieve all clusters at all possible heights. If the user prefers a specified number of clusters or a specified height, these can instead be provided.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stats]{cutree}}
#' @rdname clusters
#' @export 
#' @importFrom stats cutree
clusters = function(m = NULL,
                    hc = NULL,
                    cor.method = 'pearson',
                    hc.method = 'average',
                    compute.dist = T,
                    dist.method = 'euclidean',
                    k = NULL,
                    h = NULL,
                    clean = TRUE,
                    min.rel = 0.05,
                    max.rel = 0.8,
                    min.abs = 5) {

    # only provide one of m and hc
    stopifnot(sum(sapply(list(m, hc), is.null)) == 1)

    if (is.null(hc)) {
        hc = clusta(mat = m,
                    HC = T,
                    compute.dist = compute.dist,
                    dist.method = dist.method,
                    cor.method = cor.method,
                    hc.method = hc.method)
    }

    if (is.null(k) & is.null(h)) {
        h = hc$height
    }

    Clusters = stats::cutree(tree = hc, h = h, k = k)
    labels = rownames(Clusters)
    Clusters = Clusters %>% 
        as.data.frame %>%
        as.list %>%
        sapply(., function(clusterID) split(labels, clusterID), simplify = F) %>%
        unlist(recursive = F, use.names = F) %>%
        stats::setNames(., 1:length(.))

    if (clean) {
        Clusters = clean_by_size(clusters = Clusters,
                                 min.rel = min.rel,
                                 max.rel = max.rel,
                                 min.abs = min.abs)
    }
    
    Clusters
}

