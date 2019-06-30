
.clusters = function(m = NULL,
                     hc = NULL,
                     cor.method = 'pearson',
                     hc.method = 'average',
                     compute.dist = T,
                     dist.method = 'euclidean',
                     k = NULL,
                     h = NULL) {

    # only provide one of m and hc
    if (sum(sapply(list(m, hc), is.null)) > 1) {
        stop('Please provide one of m or hc arguments.')
    }

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
    Clusters
}


#' @title Hierarchical Clustering and Differential Gene Expression
#' @description This function computes the clusters from the expression matrix specified by hierarchical clustering. It can also (1) filter the resulting clusters by size (if they pass min and max size cutoffs), (2) compute the sets of differentially expressed genes in each cluster, (3) filter the resulting clusters by gene significance (if they pass nsig1 and nsig2 cutoffs), (4) filter the resulting clusters by cluster similarity (if they pass the Jaccard similarity cutoff) such that for any pair of clusters with too-high similarity, the least-significant cluster is removed and (4) sort the resulting clusters by significance.
#' @param m matrix. Default: NULL
#' @param hc hierarchical clustering (hclust) object. If provided, will be used instead of computed. Default: NULL
#' @param hc.method a character string indicating which agglomeration method to use. Default: 'average'
#' @param cor.method a character string indicating which correlation coefficient is to be computed. Default: 'pearson'
#' @param compute.dist a boolean value indicating whether a distance measure should be computed from the correlation metric. If FALSE, distances are computed from the correlation matrix directly. Default: T
#' @param dist.method a character string specifying the distance metric to be used for hierarchical clustering. Default: 'euclidean'
#' @param k number of clusters to return. Default: NULL
#' @param h tree height at which to retrieve clusters. Default: NULL
#' @param size.filter if TRUE, will return clusters that are within the sizes specified. Default: TRUE
#' @param min.size a boolean value or numeric value indicating whether there should be a minimum number of clusters or what the minimum number of clusters should be, respectively. If the former, then the minimum number of clusters will be calculated relative to the total number of members with min.rel.
#' @param max.size a boolean value or numeric value indicating whether there should be a maximum number of clusters or what the maximum number of clusters should be, respectively. If the former, then the maximum number of clusters will be calculated relative to the total number of members with max.rel.
#' @param min.rel minimum relative size of a cluster. Default: 0.01 (ie. 0.01 of total number of members)
#' @param max.rel maximum relative size of a cluster. Default: 0.8 (ie. 0.8 of total number of members)
#' @param min.abs absolute minimum size. If min.rel < min.abs, min.abs will replace min.rel. Default: 5
#' @param DEA a boolean value indicating whether to run Differential Gene Expression Analysis. Required if significance.filter, simCut or sortBySignificance are TRUE. Default: T
#' @param FC fold change cutoff. Passed to \code{\link[scrabble]{genes}}. Default: 2
#' @param is.log values are in log2 (used in fold_changes). Passed to \code{\link[scrabble]{genes}}. Default: T
#' @param p p-value cutoff. Passed to \code{\link[scrabble]{genes}}. Default: 0.01
#' @param adjust.method Correction for multiple tests. If not desired, set to 'none'. Passed to \code{\link[scrabble]{genes}}. Default: 'BH'
#' @param p.sort returning values are sorted by p-value (most significant first). Passed to \code{\link[scrabble]{genes}}. Default: F
#' @param return.full return cluster membership and cluster DE genes. Passed to \code{\link[scrabble]{genes}}. Default: F
#' @param return.p return cluster DE p-values. Passed to \code{\link[scrabble]{genes}}. Default: F
#' @param fast if TRUE, computes p-values for significant fold-change values only. Passed to \code{\link[scrabble]{genes}}. Default: T
#' @param significance.filter a boolean value indicating whether to remove clusters that do not pass gene significance cutoffs. Default: T
#' @param nsig1 cutoff for number of differentially expressed genes. Used to cut by significance (significance.filter = TRUE). If not desired, set to FALSE. Default: 30
#' @param nsig2 cutoff for number of most differentially expressed genes. Used to cut by significance (significance.filter = TRUE). If not desired, set to FALSE. Default: 5
#' @param similarity.filter a boolean value indicating whether to remove clusters that are too similar. Default: T
#' @param all.filters a boolean value indicating whether to set all filtering options to TRUE: size.filter, significance.filter, similarity.filter. Default: T
#' @param similarity.cutoff numeric value between 0 and 1 defining the maximum Jaccard similarity allowed. Default: 0.8
#' @param order.method character string indicating which ordering method should be used, one of 'combined', 'sequential', 'sig1' or 'sig2'. 'combined' orders by the mean of sig1 ranks and sig2 ranks. 'sequential' orders by sig1 and sig2 sequentially, ordering by the second only where there are ties in the first. 'sig1' orders by the number of elements in each vector. 'sig2' orders by the number of elements in each vector after filtering the list such that each element only appears once in the vector in which it's value is highest. Default: 'combined'
#' @param ties.method character string specifying how ties are treated, see base::rank for more details.
#' @param first character string indicating whether ordering should be first by sig1 and then by sig2 (default) or vice versa. One of 'sig1' or 'sig2'. Only relevant if order.method is 'sequential'. Default: 'sig1'
#' @param decreasing boolean value indicating whether the list should be reordered by decreasing or increasing significance. If TRUE, the most significant vector will appear first. Default: TRUE
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
                    clusters = NULL,
                    cor.method = 'pearson',
                    hc.method = 'average',
                    compute.dist = T,
                    dist.method = 'euclidean',
                    k = NULL,
                    h = NULL,
                    size.filter = F,
                    min.size = T,
                    max.size = T,
                    min.rel = 0.05,
                    max.rel = 0.8,
                    min.abs = 5,
                    DEA = T,
                    FC = 2,
                    is.log = T,
                    p = 0.01,
                    adjust.method = 'BH',
                    sort = T,
                    p.sort = F,
                    return.full = F,
                    return.p = F,
                    fast = T,
                    significance.filter = F,
                    nsig1 = 30,
                    nsig2 = 5,
                    similarity.filter = F,
                    all.filters = T,
                    similarity.cutoff = 0.8,
                    order.method = 'combined',
                    ties.method = 'random',
                    first = 'sig1',
                    decreasing = TRUE,
                    sortBySignificance = T,
                    verbose = F){

    if (all.filters) {
        size.filter = T
        significance.filter = T
        similarity.filter = T
    }

    if (is.null(clusters)) {
        Clusters = .clusters(m = m,
                             hc = hc,
                             cor.method = cor.method,
                             hc.method = hc.method,
                             compute.dist = compute.dist,
                             dist.method = dist.method,
                             k = k,
                             h = hc)
    } else {
        Clusters = clusters
    }

    if (size.filter) {
        Clusters = clean_by_size(clusters = Clusters,
                                 min.rel = min.rel,
                                 max.rel = max.rel,
                                 min.abs = min.abs)
    }
    
    if (!DEA & (significance.filter|similarity.filter)) {
        stop('Cannot cut by significance or by similarity without running DEA. Please set DEA to TRUE.')
    }


    if (!DEA) {
        return(Clusters)
    }


    if (verbose) {
        print(paste0('Running differential gene expression on ', length(Clusters), ' clusters'))
    }
    degenes = DEgenes(m = m,
                      clusters = Clusters,
                      FC = FC,
                      is.log = is.log,
                      p = p,
                      adjust.method = adjust.method,
                      sort = sort,
                      p.sort = p.sort,
                      return.full = return.full,
                      return.p = return.p,
                      fast = fast)

    if (return.full) {
        degenes$DEgenes = Map(function(x, ind) x[ind],
                                  x = degenes$fold.change,
                                  ind = degenes$DEgenes)
    } else {
        degenes = list(clusters = Clusters,
                       DEgenes = degenes,
                       fold.change.cutoff = FC,
                       p.value.cutoff = p,
                       adjust.method = adjust.method)
    }


    if (significance.filter) {
        indexes = clean_by_genes(L = degenes$DEgenes,
                                 nsig1 = nsig1,
                                 nsig2 = nsig2,
                                 return.index = TRUE)

        exclude = -1 * (length(degenes)-2):length(degenes)
        if (verbose) {
            print(paste('Excluding', length(degenes$DEgenes) - length(indexes), 'clusters with too few DE genes'))
        }
        degenes[exclude] = sapply(degenes[exclude], function(L) L[indexes], simplify = F)
    }


    if (return.p) {
        highest = F
    } else {
        highest = T
    }


    if (similarity.filter) {

        indexes = clean_by_similarity(clusters = degenes$clusters,
                                      genes = degenes$DEgenes,
                                      cutoff = similarity.cutoff,
                                      order.method = order.method,
                                      ties.method = ties.method,
                                      first = first,
                                      decreasing = FALSE,
                                      highest = highest,
                                      sortBySignificance = FALSE,
                                      return.index = TRUE)

        exclude = -1 * (length(degenes)-2):length(degenes)

        if (verbose) {
            print(paste('Excluding', length(degenes$DEgenes) - length(indexes), 'clusters with Jaccard similarity above', similarity.cutoff))
        }
        degenes[exclude] = sapply(degenes[exclude], function(L) L[indexes], simplify = F)
    }

    if (sortBySignificance) {
        if (verbose) {
            print('Sorting clusters by significance')
        }

        indexes = order_by_significance(L = degenes$DEgenes,
                                        method = order.method,
                                        ties.method = ties.method,
                                        first = first,
                                        decreasing = decreasing,
                                        highest = highest,
                                        return.index = TRUE)

        exclude = -1 * (length(degenes)-2):length(degenes)
        degenes[exclude] = sapply(degenes[exclude], function(L) L[indexes], simplify = F)
    }

    degenes
}

