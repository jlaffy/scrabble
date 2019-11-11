
#' @title DEgenes
#' @description Differential Expression between Groups
#' @param m matrix 1
#' @param m2 matrix 2. If null, m2 = m[, !group]. Defaut: NULL
#' @param group if m2 not supplied, m = m[, group]; m2 = m[, !group]. Default: NULL
#' @param is.log values are in log2 (used in fold_changes). Default: T
#' @param FC fold change cutoff. If not desired, set to FALSE. Default: 2
#' @param adjust.method Correction for multiple tests. If not desired, set to 'none'. Default: 'BH'
#' @param p p-value cutoff. If not desired, set to FALSE. Default: 0.01
#' @param sort a boolean value indicating whether result should be sorted. Default: T
#' @param p.sort returning values are sorted by p-value (most significant first). Default: F
#' @param return.full return both fold-change values (default) and p-values. Default: F
#' @param return.p return p-values. Default: F
#' @param returnAllGenes if TRUE, computes p-values for significant fold-change values only. Default: FALSE
#' @return (log2) fold-change values for rows in m, sorted by FC (highest first) and including only those that are significant according to FC and p cutoffs.
#' @details DETAILS
#' @rdname DEgenes
#' @export 
DEgenes = function(m,
                   m2 = NULL,
                   group = NULL,
                   is.log = T,
                   FC = 2,
                   adjust.method = 'BH',
                   p = 0.01,
                   sortBy = c('fc', 'p', 'none'),
                   returnVal = c('fc', 'p', 'all'),
                   returnAllGenes = FALSE) {

    # if <group> provided instead of m2
    # m is split to m[, group] and m[, setdiff(colnames(m), group)]
    stopifnot(sum(sapply(list(m2, group), is.null)) == 1)

    # step 1: Fold Changes
    step1 = .DEgenes.FCstep(m = m,
                            m2 = m2,
                            group = group,
                            FC = FC,
                            is.log = is.log,
                            returnAllGenes = returnAllGenes,
                            returnVal = returnVal)
    list2env(step1, envir = environment())

    # step 2: P-values
    step2 = .DEgenes.Pstep(m = m,
                           m2 = m2,
                           group = group,
                           adjust.method = adjust.method)
    list2env(step2, envir = environment())

    # step 3: Sort Genes (by FC or p-value, or neither)
    step3 = .DEgenes.Sort(fcs = fcs, ps = ps, sortBy = sortBy)
    list2env(step3, envir = environment())

    if (length(genesPassingFC) == 0) {
        return(genesPassingFC)
    }

    # step 4: Find Significant Genes
    step4 = .DEgenes.Significant(p = p, FC = FC, fcs = fcs, ps = ps, returnAllGenes = returnAllGenes)
    list2env(step4, envir = environment())

    # step 5: prepare output
    step5 = .DEgenes.PrepareOutput(fcs = fcs, ps = ps, ind = ind, returnVal = returnVal, returnAllGenes = returnAllGenes)
    list2env(step5, envir = environment())

    result
}
