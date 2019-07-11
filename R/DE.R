#!/usr/bin/env Rscript

#' @title DE
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
#' @param fast if TRUE, computes p-values for significant fold-change values only. Default: FALSE
#' @return (log2) fold-change values for rows in m, sorted by FC (highest first) and including only those that are significant according to FC and p cutoffs.
#' @details DETAILS
#' @rdname DE
#' @export 
DE = function(m,
              m2 = NULL,
              group = NULL,
              is.log = T,
              FC = 2,
              adjust.method = 'BH',
              p = 0.01,
              sort = T,
              p.sort = F,
              return.full = F,
              return.p = F,
              fast = FALSE) {

    # if <group> provided instead of m2
    # m is split to m[, group] and m[, setdiff(colnames(m), group)]
    stopifnot(sum(sapply(list(m2, group), is.null)) == 1)

    # (1) calculate fold change values between corresponding pairs of rows from the two matrices
    fcs = fold_changes(m = m,
                       m2 = m2,
                       group = group,
                       is.log = is.log)

    # if data in log form, convert FoldChange value to log2
    if (is.numeric(FC) & is.log) {
        FC = log2(FC)
    }

    if (is.numeric(FC) & isTRUE(fast) & !isTRUE(return.full)) {
        genesPassingFC = which(fcs >= FC)
        if (length(genesPassingFC) == 0) {
            return(genesPassingFC)
        }
        fcs = fcs[genesPassingFC]
        m = m[genesPassingFC, , drop = F]
        m2 = m2[genesPassingFC, , drop = F]
    }

    # (2) calculate p-values across corresponding pairs of rows from the two matrices
    ps = t_tests(m = m,
                 m2 = m2,
                 group = group,
                 adjust.method = adjust.method)

    if (sort) {
        # sort by p values instead of by fold_changes
        if (p.sort) {
            Order = order(ps, decreasing = F) # sort by p values (smallest first)
        } else {
            Order = order(fcs, decreasing = T) # sort by fold_changes (largest first)
        }
        
        fcs = fcs[Order]
        ps = ps[Order]
    }

    # (3) get indexes of statistically significant differences
    # as defined by the FoldChange value and the P-value
    if (is.numeric(p) & is.numeric(FC)) {
        ind = which(fcs >= FC & ps <= p)
    } else if (is.numeric(p)) {
        ind = which(ps <= p)
    } else if (is.numeric(FC)) {
        ind = which(fcs >= FC)
    } else {
        ind = stats::setNames(1:length(fcs), names(fcs))
    }

    if (return.full) {
        return(list(fold.change = fcs, p.value = ps, DEgenes =ind))
    }

    # return p values instead of fold_changes?
    if (return.p) {
        result = stats::setNames(ps[ind], names(fcs)[ind]) # return p values
    } else {
        result = stats::setNames(fcs[ind], names(fcs)[ind]) # return fold_changes
    }

    result
}
