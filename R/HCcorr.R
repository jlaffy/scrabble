
HCcorr = function(m = NULL,
                  cr = NULL,
                  flip.rows = F,
                  flip.cols = F,
                  order.cols = T,
                  order.rows = T,
                  ...) {

    if (!isTRUE(order.cols) & !isTRUE(order.rows)) {
        message('Neither columns nor rows of output matrix will be ordered.')
    }

    if (is.null(cr)) {
        cr = F
    }

    result = HC(m = m, cr = cr, ord = T, return.steps = T, ...)
    cr = result$cr
    colOrd = result$ord
    rowOrd = colOrd
    if (flip.cols) colOrd = rev(colOrd)
    if (flip.rows) rowOrd = rev(rowOrd)

    if (!order.rows) return(cr[, colOrd])
    if (!order.cols) return(cr[rowOrd, ])
    cr[rowOrd, colOrd]
}
