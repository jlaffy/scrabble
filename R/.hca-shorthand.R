
#' hca wrappers
#' @description Use hca_* wrappers as a shorthand to retrieve specific objects from hca()
. Take care to pass (one of) the correct argument(s) to ...:
#' @description hca_cr -- <m>  
#' @description hca_dst -- <m> or <cr>  
#' @description hca_hc -- <m>, <cr> or <dst>  
#' @description hca_ord --  <m>, <cr>, <dst> or <hc>  
#' @description hca_clusters -- any of the above.
#' @param ... see arguments in hca for more details. 
#' @return object produced in call to hca. One of: correlation matrix, distance matrix, hclust object, column order, list of column clusters
#' @examples hca.clusters(hc = hc)
#' @examples hca.ord(m = m)

#' @rdname hcaWrappers
#' @export 
hca_cr = function(m) {
    hca(m = m, cr = T, return.steps = F)
}

#' @rdname hcaWrappers
#' @export 
hca_dst = function(...) {
    hca(dst = T, return.steps = F, ...)
}

#' @rdname hcaWrappers
#' @export 
hca_hc = function(...) {
    hca(hc = T, return.steps = F, ...)
}


#' @rdname hcaWrappers
#' @export 
hca_ord = function(...) {
    hca(ord = T, return.steps = F, ...)
}

#' @rdname hcaWrappers
#' @export 
hca_clusters = function(...) {
    hca(clusters = T, return.steps = F, ...)
}
