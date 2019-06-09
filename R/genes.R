
#' @title genes
#' @description Differentially Expressed Genes of Statistically Significant Clusters
#' @param m matrix
#' @param hc hierarchical clustering (hclust) object. If provided, will be used instead of computed. Default: NULL
#' @param clusters List of cluster memberships (character vectors). If provided, will be used instead of computed.. Default: NULL
#' @param FC fold change cutoff. Default: 2
#' @param is.log values are in log2 (used in fold_changes). Default: T
#' @param p p-value cutoff. Default: 0.01
#' @param adjust.method Correction for multiple tests. If not desired, set to 'none'. Default: 'BH'
#' @param p.sort returning values are sorted by p-value (most significant first). Default: F
#' @param return.all return cluster membership and cluster DE genes. Default: F
#' @param return.p return cluster DE p-values. Default: F
#' @param fast if TRUE, computes p-values for significant fold-change values only. Default: T
#' @param clean return only significant clusters as defined by nsig1 AND nsig2. Default: TRUE
#' @param nsig1 cutoff for number of differentially expressed genes. Default: 30
#' @param nsig2 cutoff for number of most differentially expressed genes. Default: 5
#' @param Names if TRUE, will return only gene names. Default: F
#' @param return.clusters if TRUE, will return cluster memberships as well as cluster genes. Default: F
#' @param ... other arguments passed to \code{clusters}.
#' @return with default parameters, will return a list of differentially expressed genes in statistically significant clusters. Differentially expressed genes are those passing fold change and p-value cutoffs. Statistically significant clusters are those passing nsig1 and nsig2 cutoffs. Within clusters, gene values displayed are fold changes and genes are ordered by highest FC first. 
#' @details DETAILS
#' @rdname genes
#' @export 
genes = function(m,
                 hc = NULL,
                 clusters = NULL,
                 FC = 2,
                 is.log = T,
                 p = 0.01,
                 adjust.method = 'BH',
                 p.sort = F,
                 return.all = F,
                 return.p = F,
                 fast = T,
                 clean = TRUE,
                 nsig1 = 30,
                 nsig2 = 5,
                 Names = F,
                 return.clusters = F,
                 ...) {

    if (is.null(clusters)) {
        Clusters = clusters(m = m, hc = hc, ...)
    }

    clusterGenes = sapply(Clusters, function(group) {
                              DE(m = m,
                                 group = group,
                                 FC = FC,
                                 is.log = is.log,
                                 p = p,
                                 adjust.method = adjust.method,
                                 p.sort = p.sort,
                                 return.all = return.all,
                                 return.p = return.p,
                                 fast = fast)},
                          simplify = F)

    if (clean) {
        clusterGenes = clean_by_genes(clusterGenes, nsig1 = nsig1, nsig2 = nsig2)
    }

    if (Names) {
        clusterGenes = sapply(clusterGenes, names, simplify = F)
    }

    if (return.clusters) {
        return(list(clusters = Clusters[names(clusterGenes)],
                    genes = clusterGenes))
    }

    clusterGenes
}
