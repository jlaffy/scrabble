
#' @title Differential Expression Analysis
#' @description differential expression analysis 
#' @param x matrix or character vector with matrix column names
#' @param y matrix to compare against or matrix with x and y column names
#' @param is.log is the expression data in log2 space? Default: T
#' @param cutoff.fc numeric value for fold change gene cutoffs (not in log space) or NULL. Default: 2
#' @param cutoff.p numeric value for p-value gene cutoffs (not in log space) or NULL. Default: 0.01
#' @param adjust.method correction for multiple testing? 'none' if not desired. Default: unique(c("BH", stats::p.adjust.methods))
#' @param sortBy parameter to sort the genes returned by. One of fold change (default), p-value or none, in which case the original order of the rows in the matrix is kept. Default: c("fc", "p", "none")
#' @param returnValue parameter describing the genes to be returned. One of fold change (default), p-value or all,  in which case fold changes, p-values and genes considered 'DE' (ie if cutoffs are provided) are returned.Default: c("fc", "p", "all")
#' @param returnAllGenes should all genes be returned instead of only those passing cutoffs? Default: FALSE
#' @return if returnValue == 'all', a list, else a named numeric vector. If a named numeric vector, names are gene names (row names) and values are fold changes if returnValue == 'fc' or p values of returnValue == 'p'. The number of genes returned depends on whether returnAllGenes == T and whether cutoff.fc and cutoff.p were given (default is that they are).
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
               adjust.method = unique(c('BH', stats::p.adjust.methods)),
               sortBy = c('fc', 'p', 'none'),
               returnValue = c('fc', 'p', 'all'),
               returnAllGenes = FALSE) {

    adjust.method = match.arg(adjust.method)
    sortBy = match.arg(sortBy)
    returnValue = match.arg(returnValue)

    if (!returnAllGenes) {
        fc = foldchange(x, y, is.log = is.log, cutoff = cutoff.fc)
        p = ttest(x[names(fc)], y[names(fc)], adjust.method = adjust.method, cutoff = NULL)
    } else {
        fc = foldchange(x, y, is.log = is.log, cutoff = NULL)
        p = ttests(x, y, adjust.method = adjust.method, cutoff = NULL)
    }

    if (is.null(cutoff.fc)) cutoff.fc = rep(T, length(fc))
    else cutoff.fc = fc >= cutoff.fc
    if (is.null(cutoff.p)) cutoff.p = rep(T, length(fc))
    else cutoff.p = p <= cutoff.p
    gene.bool = cutoff.fc & cutoff.p

    if (sortBy != 'none') {
        if (sortBy == 'fc') {
            sortBy = 1
            decreasing = T
        } else if (sortBy == 'p') {
            sortBy = 2
            decreasing = F
        } else stop('<sortBy> not recognised.')

        c(fc, p, gene.bool) %<-% sort_by(fc, p, gene.bool, which = sortBy, decreasing = decreasing)
    }

    genes = names(fc)[gene.bool]
    if (returnValue == 'all') return(list(degenes = genes, fc = fc, p = p))
    else if (returnValue == 'p') return(p)
    fc
}
