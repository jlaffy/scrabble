#' @title Differential Expression Analysis
#' @description Differential Expression Analysis. Compute gene-level fold changes and (adjusted) p-values between cell clusters of an expression matrix.
#' @param x matrix or character vector with matrix column names
#' @param y matrix to compare against or matrix with x and y column names
#' @param is.log logical indicating whether expression data is in log2 space. Fold change cutoff and calculation are changed accordingly. Default: T
#' @param fc fold change cutoff. Set to NULL to not filter genes based on fold change. Default: 2
#' @param p p value cutoff. Set to NULL to not filter genes based on p-values. Default: 0.05
#' @param use.wilcox logical value to use Wilcoxon-Mann-Whitney test instead of Student's t-test. Default: F
#' @param adjust.method correction for multiple testing? 'none' if not desired. Default: unique(c("BH", stats::p.adjust.methods))
#' @param sort.by sort output by 'fc' or 'p' value (best first). Set sort.by = NULL to preserve original gene (row) order. Default: 'fc'
#' @param output output 'fc' values or 'p' values (per gene). If output = NULL, a list containing both is returned. Default: 'fc' 
#' @return a numeric vector of gene fold changes or p-values or a list containing both. skip.genes = F to return information on every gene in the expression matrix (regardless of it passing cutoffs). 
#' @seealso 
#'  \code{\link[stats]{character(0)}}
#' @rdname dea
#' @export 
#' @importFrom stats p.adjust.methods
dea = function(x,
               y,
               is.log = T,
               fc = 2L,
               p = 0.05,
               use.wilcox = F,
               adjust.method = unique(c('BH', stats::p.adjust.methods)),
               sort.by = c('fc', 'p'),
               output = c('fc', 'p')) {

    adjust.method = match.arg(adjust.method)
    sort.by = match.arg(sort.by)
    output = match.arg(output)
    if (use.wilcox) ttest.fun = wilcoxtest
    else ttest.fun = ttest

    FC = foldchange(x, y, is.log = is.log, cutoff = fc)
    if (length(FC) == 0) stop('No genes passed fold change cutoff')
    if (length(FC) < nrow(y)) {
        if (has_dim(x)) x = x[names(FC), , drop = F]
        y = y[names(FC), , drop = F]
    }

    P = ttest.fun(x, y, adjust.method = adjust.method, cutoff = p)
    FC = FC[names(FC) %in% names(P)]

    if (!is.null(sort.by)) {
        if (sort.by == 'fc') {
            sort.by = 1
            decreasing = T
        } else if (sort.by == 'p') {
            sort.by = 2
            decreasing = F
        } else stop('<sort.by> not recognised.')
        c(FC, P) %<-% sort_by(FC, P, which = sort.by, decreasing = decreasing)
    }

    if (is.null(output)) return(list(fc = FC, p = P))
    else if (output == 'p') return(P)
    else if (output != 'fc') warning('<output> not recognised. Reverting to default <output> option: "fc"')
    FC
}
