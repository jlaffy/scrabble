

sort_by = function(..., which = 1, decreasing = T) {
    dots = list(...)
    if (any(which > length(dots))) stop('<which> vector index is larger than the number of vectors.')
    orderers = sapply(which, function(i) dots[[i]], simplify = F)
    Order = do.call(order, c(orderers, list(decreasing = decreasing)))
    sapply(1:length(dots), function(i) dots[[i]][Order], simplify = F)
}


is_p_value = function(x) {
    !is.null(x) && is.numeric(x) && x >= 0 & x <= 1
}

have_equal_nrows = function(m1, m2) {
    nrow(m1) == nrow(m2)
}

have_equal_rownames = function(m1, m2) {
    all(rownames(m1) == rownames(m2))
}

has_dim <- function(x) {
  !is.null(attr(x, "dim"))
}

split_matrix = function(m, by) {
    stopifnot(has_dim(m))
    stopifnot(is.character(by))
    stopifnot(all(by %in% colnames(m)))
    list(x = m[, by], y = m[, !colnames(m) %in% by])
}

is_number = function(x) {
    is.numeric(x) & length(x) == 1
}


#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%->\%}} for details.
#' @importFrom zeallot %->%
#' @export
#' @rdname unpack-assign-back
#' @name %->%
#' @keywords internal
`%->%`

#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%<-\%}} for details.
#' @importFrom zeallot %<-%
#' @export
#' @rdname unpack-assign
#' @name %<-%
#' @keywords internal
`%<-%`
