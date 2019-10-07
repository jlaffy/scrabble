
HCmatrix = function(m, order.cols = T, order.rows = T, ...) {
    if (order.cols) m = m[, HC(m, ord = T, return.steps = F, ...)]
    if (order.rows) m = m[HC(t(m), ord = T, return.steps = F, ...), ]
    m
}

