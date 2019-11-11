
corder = function(m, col = T, row = T) {
    if (is.corr.matrix(m)) {
        warning('Check that <m> is not already a correlation matrix...')
    }
    obj = HC(m = m)
    cr = obj$cr
    ord = obj$ord
    if (col) cr = cr[, ord]
    if (row) cr = cr[ord, ]
    cr
}


