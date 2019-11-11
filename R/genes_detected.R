
genes.detected = function(m) {
    m = as.matrix(m)
    res = matrixStats::colCounts(m != 0)
    stats::setNames(res, colnames(m))
}
