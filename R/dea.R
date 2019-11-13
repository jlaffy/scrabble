#' @title Differential Expression Analysis
#' @description Differential Expression Analysis. Compute gene-level fold changes and (adjusted) p-values between cell clusters of an expression matrix.
#' @param x matrix or character vector with matrix column names
#' @param y matrix to compare against or matrix with x and y column names
#' @param is.log is the expression data in log2 space? Default: T
#' @param cutoff.fc numeric value for fold change gene cutoffs (not in log space) or NULL. Default: 2
#' @param cutoff.p numeric value for p-value gene cutoffs (not in log space) or NULL. Default: 0.01
#' @param use.wilcox logical value to use Wilcoxon-Mann-Whitney test instead of Student's t-test. Default: F
#' @param adjust.method correction for multiple testing? 'none' if not desired. Default: unique(c("BH", stats::p.adjust.methods))
#' @param sort.by sort output by 'fc' or 'p' value (best first). Set sort.by = NULL to preserve original gene (row) order. Default: 'fc'
#' @param output output 'fc' values or 'p' values (per gene). If output = NULL, a list containing both is returned. Default: 'fc' 
#' @param skip.genes set to FALSE if you require fold change and/or p-values for every gene in the matrix. Keep as TRUE if you only need those values for genes that passed cutoffs. Skipping genes is faster. Default: TRUE
#' @return a numeric vector of gene fold changes or p-values or a list containing both. skip.genes = F to return information on every gene in the expression matrix (regardless of it passing cutoffs). 
#' @seealso 
#'  \code{\link[stats]{character(0)}}
#' @rdname dea
#' @export 
#' @importFrom stats p.adjust.methods
dea = function(x,
               y,
               is.log = T,
               cutoff.fc = 2,
               cutoff.p = 0.01,
               use.wilcox = F,
               adjust.method = unique(c('BH', stats::p.adjust.methods)),
               sort.by = c('fc', 'p'),
               output = c('fc', 'p'),
               skip.genes = TRUE) {

    adjust.method = match.arg(adjust.method)
    sort.by = match.arg(sort.by)
    output = match.arg(output)
    if (use.wilcox) ttest.fun = wilcoxtest
    else ttest.fun = ttest

    if (!skip.genes) {
        fc = foldchange(x, y, is.log = is.log, cutoff = NULL)
        p = ttest.fun(x, y, adjust.method = adjust.method, cutoff = NULL)
    } else {
        fc = foldchange(x, y, is.log = is.log, cutoff = cutoff.fc)
        invisible(fc)
        if (length(fc) == 0) stop('No genes left for the t-test')
        if (has_dim(x)) x = x[names(fc), , drop = F]
        y = y[names(fc), , drop = F]
        p = ttest.fun(x, y, adjust.method = adjust.method, cutoff = cutoff.p)
    }

#     # genes that pass
#     if (is.null(cutoff.fc)) cutoff.fc = rep(T, length(fc))
#     else cutoff.fc = fc >= cutoff.fc
#     if (is.null(cutoff.p)) cutoff.p = rep(T, length(fc))
#     else cutoff.p = p <= cutoff.p
#     gene.bool = cutoff.fc & cutoff.p

    if (!sort.by) {
        if (sort.by == 'fc') {
            sort.by = 1
            decreasing = T
        } else if (sort.by == 'p') {
            sort.by = 2
            decreasing = F
        } else stop('<sort.by> not recognised.')
#        c(fc, p, gene.bool) %<-% sort_by(fc, p, gene.bool, which = sort.by, decreasing = decreasing)
        c(fc, p) %<-% sort_by(fc, p, which = sort.by, decreasing = decreasing)
    }

#    genes = names(fc)[gene.bool]
    if (output == 'both') return(list(fc = fc, p = p))
    else if (output == 'p') return(p)
    fc
}
