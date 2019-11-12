
annomaps = function(..., titles = NULL, flip = F, ratio = 0.03, mar = 0.015) {
    .annomap = function(X, pal, title = NULL, x.breaks = NULL) {
        annomap(X, title = title, flip = flip, ratio = ratio, x.breaks = x.breaks, pal = pal)
    }
    dots = list(...)
    len = length(dots) - 1
    if (is.null(titles)) titles = rep('', len+1)
    maps = sapply(1:len, function(i) .annomap(X = dots[[i]], pal = i, title = titles[[i]]), simplify = F)
    lastmap = list(.annomap(dots[[len+1]], title = titles[[len+1]], pal = len+1, x.breaks = ggplot2::waiver()))
    maps = c(maps, lastmap)
    maps = sapply(maps, function(p) p + ggplot2::theme(plot.margin = margin(mar,mar,mar,mar,"cm")), simplify = F)
    if (flip) by = 'y'
    else by = 'x'
    do.call(grombine, c(... = maps, list(by = by)))
}
