
#' @title Differential Gene Expression Analysis across Cell Clusters
#' @description This function returns the genes differentially expressed in a given cluster of cells relative to the remaining cells in the matrix. If a list of clusters is provided, DEgenes will be computed for each cluster in turn. Differential expression is calculated according to fold changes and p-values. The user can specify the fold change and p-value with which genes are deemed (DE-)significant. With default arguments, genes are considered significant if FC >= 2 and p-value
#' (Benjamin-Hochberg-adjusted) <= 0.01. Also with default arguments, fold-change values are returned and genes are sorted by fold-change (largest first). The user can instead ask for genes to be sorted by p-value (most significant first) or for p-values to be returned or both. Alternatively, if the user requires fold change values and p-values for all genes (not just those that are significant), they can set return.full = TRUE. This parameter will return a list containing (1) all clusters, (2) fold-change values for all genes, (3) p-values for all genes, (4) the gene names and indices of the DE-significant genes and (5-7) the fold change and p-value cutoffs used as well as the method used to adjust p-values for multiple testing. 
#' @param m matrix
#' @param clusters List of cluster memberships (character vectors). If provided, will be used instead of computed.. Default: NULL
#' @param FC fold change cutoff. Default: 2
#' @param is.log values are in log2 (used in fold_changes). Default: T
#' @param p p-value cutoff. Default: 0.01
#' @param adjust.method Correction for multiple tests. If not desired, set to 'none'. Default: 'BH'
#' @param sort returning values are sorted by fold change (largest first). Default: T
#' @param p.sort returning values are sorted by p-value (most significant first). Default: F
#' @param return.full return cluster membership and cluster DE genes. Default: F
#' @param return.p return cluster DE p-values. Default: F
#' @param fast if TRUE, computes p-values for significant fold-change values only. Default: T
#' @return with default parameters, will return a list of differentially expressed genes in statistically significant clusters. Differentially expressed genes are those passing fold change and p-value cutoffs. Statistically significant clusters are those passing nsig1 and nsig2 cutoffs. Within clusters, gene values displayed are fold changes and genes are ordered by highest FC first. 
#' @details DETAILS
#' @rdname genes
#' @export 
DEgenes = function(m,
                   clusters,
                   FC = 2,
                   is.log = T,
                   p = 0.01,
                   adjust.method = 'BH',
                   sort = T,
                   p.sort = F,
                   return.full = F,
                   return.p = F,
                   fast = T) {

    # if only one cluster was provided and is a character vector instead of a list of length one
    # convert to a list
    if (all(lengths(clusters) == 1)) {
        clusters = list(clusters)
    }

    # if (verbose) {
    #     print(paste0('Performing differential expression analysis on ', length(Clusters), ' clusters'))
    # }

    clusterGenes = sapply(clusters, function(group) {
                              DE(m = m,
                                 group = group,
                                 FC = FC,
                                 is.log = is.log,
                                 p = p,
                                 adjust.method = adjust.method,
                                 sort = sort,
                                 p.sort = p.sort,
                                 return.full = return.full,
                                 return.p = return.p,
                                 fast = fast)},
                          simplify = F)

    if (return.full) {
        clusterGenes = list(clusters = clusters,
                            fold.change = sapply(clusterGenes, `[[`, 'fold.change', simplify = F),
                            p.value = sapply(clusterGenes, `[[`, 'p.value', simplify = F),
                            DEgenes = sapply(clusterGenes, `[[`, "DEgenes", simplify = F),
                            fold.change.cutoff = FC,
                            p.value.cutoff = p,
                            adjust.method = adjust.method)

        return(clusterGenes)
    }

    clusterGenes

}
