
#' @title t_tests
#' @description T-test between pairs of rows from two matrices
#' @param m matrix 1
#' @param m2 matrix 2. If null, m2 = m[, !group]. Defaut: NULL
#' @param group if m2 not supplied, m = m[, group]; m2 = m[, !group]. Default: NULL
#' @param adjust.method Correction for multiple tests. If not desired, set to 'none'. Default: 'BH'
#' @param MARGIN matrix margin to apply t-test by. If set to 2, T-test instead between pairs of columns. Default: 1
#' @return numeric vector of (adjusted) p-values
#' @details DETAILS
#' @seealso 
#'  \code{\link[stats]{t.test}},\code{\link[stats]{p.adjust}}
#' @rdname t_tests
#' @export 
#' @importFrom stats t.test p.adjust
t_tests = function(m,
                   m2 = NULL,
                   group = NULL,
                   adjust.method = 'BH',
                   MARGIN = 1) {

    # apply t.test function to pairs of rows from two matrices
    # or to pairs of columns if MARGIN == 2
    # if NOT to adjust for multiple corrections
    # then set adjust.method = 'none'

    if (MARGIN == 2) {
        m = t(m)
        if (!is.null(m2)) {
            m2 = t(m2)
        }
    }

    if (is.null(m2)) {
        stopifnot(!is.null(group) & all(group %in% colnames(m)))
        m2 = m[, setdiff(colnames(m), group), drop = F]
        m = m[, group, drop = F]
    }

    stopifnot(nrow(m) == nrow(m2))
    p = sapply(1:nrow(m), function(i) stats::t.test(m[i, ], m2[i, ])$p.value)
    p = stats::p.adjust(p, method = adjust.method)
    p = setNames(p, rownames(m))
    p[is.nan(p)] = 1
    p
}

