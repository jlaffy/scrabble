.prepare_mat <- function(mat, FUN = rowMeans) {
    # If x is a matrix (rows by cols), transform x to row or column means
    if (!is.null(dim(mat))) x = FUN(mat)
    # If x is numeric, it will first be sorted.
    if (is.numeric(x)) x = sort(x)
    return(x)
}


.match_bins <- function(Group, bins) {
    .check_arg(arg = Group)
    .check_missing(Group = Group, ref = bins)

    binID = bins[Group]
    binmatches = sapply(binID, function(id) names(bins)[bins == id], simplify = F)
    binmatches
}


.sample_bins <- function(bins,
                        n = 100,
                        replace = FALSE) {

    .check_sample_size(bins = bins, n = n, replace = replace)
    sapply(bins, function(bin) {
               sample(bin, size = n, replace = replace) },
               simplify = F) %>% 
        unlist(use.names = F)
}


binmatch <- function(Group,
                     mat = NULL,
                     x = NULL,
                     bins = NULL,
                     nbin = 30,
                     n = 100,
                     replace = FALSE,
                     ...) {
    .check_args_exist(x = x, mat = mat, bins = bins)

    if (!is.null(x) | !is.null(mat)) {
        bins = bin(x = x, mat = mat, breaks = nbin, ...)
    }
    
    binmatches = .match_bins(Group = Group, bins = bins)
    binsamples = .sample_bins(bins = binmatches, n = n, replace = replace)
    binsamples
}


#' @title Generate Equally-Sized Bins
#' @description This function generates equally-sized bins from a named numeric vector or from a summary statistic of a matrix's rows. If a matrix is provided, the numeric vector will first be computed; it will be of the same length as there are rows in the matrix, and each value will represent a summary statistic of each of the rows, the default being row means.
#' @param mat a matrix over whose rows to calculate a summary statistic and subsequently generate bins from. Default: NULL
#' @param x a named numeric vector to generate bins from. Default: NULL
#' @param breaks the number of desired bins. Default: 30
#' @param FUN function to be applied to each row of a matrix. Must return a single numeric value per row. Default: rowMeans
#' @return a named numeric vector, with as names the names of <x> or the rownames of <mat> and as values the bin IDs computed. If names(x) or rownames(mat) are NULL, the vector names will instead be the values that were binned (e.g. the row means)
#' @rdname bin
#' @export 
bin <- function(mat = NULL,
                x = NULL,
                breaks = 30,
                FUN = rowMeans) {
    if (!is.null(mat)) x = .prepare_mat(mat, FUN = FUN)    
    .check_arg(arg = x)
    binIDs = cut(seq_along(x), breaks = breaks, labels = F, include.lowest = T)
    .name_binIDs(binIDs = binIDs, x = x)
}
