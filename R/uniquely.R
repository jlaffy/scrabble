
#' @title uniquely
#' @description unique instances of names (Genes) across a list of named numeric vectors
#' @param L List of named numeric vectors (e.g. Gene names and FC values for different clusters)
#' @param Names return just names (Genes), Default: F
#' @param highest if TRUE, unique instance is equal to highest instance, else to lowest. Default: T
#' @return List of named numeric vectors, with only one instance of each name across all vectors
#' @details DETAILS
#' @examples 
#' @seealso 
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}
#' @rdname uniquely
#' @export 
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate arrange desc
uniquely = function(L, Names = F, highest = T) {
    stopifnot(!is.null(names(L)))

    dat = cbind.data.frame(reshape2::melt(L), reshape2::melt(sapply(L, names, simplify = F)))

    # remove duplicate column 4 and reorder and rename columns
    dat = dat[, -4][, c(2, 3, 1)]
    colnames(dat) <- c('cluster', 'gene', 'value')

    # make cluster column factor, such that it will remain in final list even if containing nothing
    dat = dplyr::mutate(dat, cluster = factor(cluster, levels = names(L)))

    # order by smallest
    if (!highest) {
        dat  = dplyr::arrange(dat, gene, value)
    # order by highest
    } else {
        dat = dplyr::arrange(dat, gene, dplyr::desc(value))
    }
    # remove duplicates after ordering
    dat = dat[!duplicated(dat$gene), ]

    # split resulting unique genes by clusters
    result = Map(stats::setNames, split(dat$value, dat$cluster), split(dat$gene, dat$cluster))[names(L)]

    # return gene names without gene values
    if (Names) return(sapply(result, names, simplify = F))

    # return named gene values by cluster
    # clusters ordered as in argument
    result
}


