
#' @title Order List of Named Gene Vectors by Significance
#' @description Reorders a list of named numeric vectors according to two parameters for significance, sig1 and sig2. sig1 is the number of elements in each vector. sig2 is the number of elements in each vector after filtering the list such that each element only appears once in the vector in which it's value was highest.
#' @param L List of named numeric vectors (e.g. Gene names and FC values for different clusters)
#' @param method character string indicating which ordering method should be used, one of 'combined', 'sequential', 'sig1' or 'sig2'. 'combined' orders by the mean of sig1 ranks and sig2 ranks. 'sequential' orders by sig1 and sig2 sequentially, ordering by the second only where there are ties in the first. 'sig1' orders by the number of elements in each vector. 'sig2' orders by the number of elements in each vector after filtering the list such that each element only appears once in the vector in which it's value is highest. Default: 'combined'
#' @param character string specifying how ties are treated, see base::rank for more details.
#' @param first character string indicating whether ordering should be first by sig1 and then by sig2 (default) or vice versa. One of 'sig1' or 'sig2'. Only relevant if method is 'sequential'. Default: 'sig1'
#' @param decreasing boolean value indicating whether the list should be reordered by decreasing or increasing significance. If TRUE, the most significant vector will appear first. Default: TRUE
#' @param return.index boolean value indicating whether ordering index vector should be returned instead of sorted list. Default: FALSE
#' @return List of named gene vectors reordered by significance. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname order_by_significance
#' @export 
order_by_significance = function(L,
                                 method = c("combined", "sequential", "sig1", "sig2"),
                                 ties.method = c("average", "first", "last", "random", "max", "min"),
                                 first = c("sig1", "sig2"),
                                 decreasing = TRUE,
                                 return.index = FALSE) {
     
    if (length(method) > 1) method = method[1]
    if (length(ties.method) > 1) ties.method = ties.method[4]
    if (length(first) > 1) first = first[1]

    sig1 = lengths(L)
    sig2 = lengths(uniquely(L))
    rank1 = rank(sig1, ties.method = ties.method)
    rank2 = rank(sig2, ties.method = ties.method)

    if (method == 'combined') {
        l = rank(rank1 + rank2, ties.method = ties.method)
        Order = order(l, decreasing = decreasing)
    }

    else if (method == 'sequential') {
        if (first == 'sig1') {
            Order = order(sig1, sig2, decreasing = decreasing)
        } 
        else if (first == 'sig2') {
            Order = order(sig2, sig1, decreasing = decreasing)
        }
        else stop('first must equal one of "sig1" or "sig2"')
    }

    else if (method == 'sig1') {
        Order = order(rank1, decreasing = decreasing)
    }

    else if (method == 'sig2') {
        Order = order(rank2, decreasing = decreasing)
    }

    else stop('method must equal one of "combined", "sequential", "sig1" or "sig2"')

    if (return.index) {
        return(Order)
    }
    
    L[Order]
}


