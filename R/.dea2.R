
make_fac = function(vars, x) {
    factor(as.numeric(vars %in% x) + 1)
}

.dea = function(m, x) {
    res = rowttest(m, x)[,c("dm", "p.value")]
    colnames(res) = c("fc", "p")
}

setMethod(f = 'rowttests', signature = c('matrix', 'character'),
)
