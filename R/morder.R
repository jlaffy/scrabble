
.morder.cormat = function(m, col = T, row = T) {
    ord = HC(cr = m, ord = T)
    if (col) m = m[, ord]
    if (row) m = m[ord, ]
    m
}

morder = function(m, col = T, row = T) {
    if (is.simil.matrix(m)) {
        return(.morder.cormat(m, col = col, row = row))
    }
    if (col) m = m[, HC(m = m, ord = T)]
    if (row) m = m[HC(m = t(m), ord = T), ]
    m
}
