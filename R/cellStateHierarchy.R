#!/usr/bin/env Rscript

cellStateHierarchy = function(m, states = 4, xl = 'AC', xr = 'MES', yl = 'OPC', yr = 'NPC') {
    d = as.data.frame(m)
    rows = rownames(d)
    d1 = d[, sapply(list(xl, xr, yl, yr), function(nam) which(colnames(d) == nam))]
    colnames(d1) = c('xl', 'xr', 'yl', 'yr')
    d2 = dplyr::transmute(d,
                          lr1 = pmax(xl, xr),
                          lr2 = pmax(yl, yr),
			  l_v_r1 = xr - xl,
			  l_v_r2 = yr - yl,
                          x0 = ifelse(lr1 > lr2, l_v_r1, l_v_r2),
                          y0 = lr2 - lr1,
                          x = sign(x0) * log2(abs(x0) + 1),
                          y = sign(y0) * log2(abs(y0) + 1))
    d3 = dplyr::transmute(d2, X = x, Y = y)
    rownames(d3) = rows
    d3
}
