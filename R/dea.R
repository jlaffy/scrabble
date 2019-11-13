
.as_fac = function(x, cols) {
    factor(as.numeric(cols %in% x) + 1)
}

#' @title Differential Expression Analysis
#' @description Differential Expression Analysis. Compute gene-level fold changes and (adjusted) p-values between cell clusters of an expression matrix.
#' @param m numeric matrix. The matrix should not contain NA values.
#' @param x the group to be tested. This can be supplied as a character vector, or as a factor containing two levels where the first corresponds to the group.
#' @param is.log is the data in log2 space? Fold change cutoff and calculation are adjusted accordingly. Default: T
#' @param fc fold change cutoff. Set to NULL to not filter genes based on fold change. Default: 2
#' @param p p value cutoff. Set to NULL to not filter genes based on p-values. Default: 0.05
#' @param p.adjust.method correction for multiple testing? One of stats::p.adjust.methods, or 'none' if not desired. Default: 'BH'
#' @param sort.by sort output by 'fc' or 'p' value (best first). Set sort.by = NULL to preserve original gene (row) order. Default: 'fc'
#' @param output output 'fc' values or 'p' values (per gene). If output = NULL, the full dataframe is returned. Default: 'fc' 
#' @return a numeric vector of gene fold changes or p-values or a dataframe with both.
#' @seealso 
#'  \code{\link[stats]{character(0)}}
#' @rdname dea
#' @export 
#' @importFrom stats p.adjust.methods
dea = function(m,
               x,
               is.log = T,
               fc = 2L,
               p = 0.05,
               p.adjust.method = 'BH',
               sort.by = c('fc', 'p'),
               output = c('fc', 'p')) {

    if (is.log & !is.null(fc)) fc = log2(fc)
    p.adjust.method = match.arg(adjust.method)
    sort.by = match.arg(sort.by)
    output = match.arg(output)

    if (!is.factor(x)) x = .as_fac(x, cols = colnames(m))
    res = genefilter::rowttests(m, fac = x)[, c("dm", "p.value")]
    res = tibble::rownames_to_column(res, 'gene')
    colnames(res)[2] = 'fc'

    res = dplyr::mutate(res, p.value = stats::p.adjust(p.value, method = p.adjust.method))
    if (!is.null(fc)) res = dplyr::filter(res, fc >= fc)
    if (!is.null(p)) res = dplyr::filter(res, p.value <= p)

    if (!is.null(sort.by)) {
        if (sort.by == 'fc') res = dplyr::arrange(res, desc(fc))
        else if (sort.by == 'p') res = dplyr::arrange(res, p.value)
        else stop('<sort.by> not recognised.')
    }

    if (is.null(output)) return(res)
    else if (output == 'p') return(stats::setNames(res$p.value, res$gene))
    else if (output != 'fc') warning('<output> not recognised. Reverting to default: "fc"')
    stats::setNames(res$fc, res$gene)
}
