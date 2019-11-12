
annomap = function(X,
                   num = T,
                   title = 'test',
                   x.title = 'x axis', 
                   breaks = ggplot2::waiver(),
                   cols = NULL,
                   cols.order = names(cols),
                   legend.pos = 'none',
                   flip = F,
                   ratio = 0.05) {

    # fix dataframe
    if (!is.null(dim(X))) {
        X = as.data.frame(X)
        colnames(X) = c('x', 'fill')
    }
    # OR make dataframe
    else if (is.atomic(X)) {
        stopifnot(!is.null(names(X)))
        if (!is.null(levels(X))) X = X[match(X, levels(X))]
        X = data.frame(x = X, fill = names(X))
    }

    # internal plotting vars
    expand = c(0, 0)
    x.title.pos = 'top'
    y.title.pos = 'left'
    col.groups = unique(X$fill)
    if (is.null(cols)) cols = rainbow(n = length(col.groups))
    if (!is.null(cols.order)) cols = cols[match(col.groups, cols.order)]
    # adjust for flipped plot
    if (flip) {
        ratio = 1/ratio
        y.title.pos = 'right'
    }

    # ggplot x scale
    if (num) X = dplyr::mutate(X, x = as.numeric(x))
    if (class(X$x) == 'numeric') scale_x_choose = ggplot2::scale_x_continuous
    else scale_x_choose = ggplot2::scale_x_discrete

#    x.scale = quote(scale_x_choose(expand = expand, breaks = breaks))
    # ggplot y scale
#     y.scale = quote(ggplot2::scale_y_continuous(expand = expand,
#                                                 breaks = NULL,
#                                                 position = y.title.pos))

    invisible(X)

    # plot
    G = quote(ggplot2::ggplot(X, aes(x = x, fill = fill, y = 1)) +
                ggplot2::geom_tile() + 
                ggplot2::theme_bw() +
                ggplot2::theme(aspect.ratio = ratio,
                               axis.title.y = ggplot2::element_text(size = rel(1.2)),
                               legend.position = legend.pos) +
#                eval(x.scale) +
#                eval(y.scale) +
                scale_x_choose(expand = expand,
                               breaks = NULL,
                               position = x.title.pos) +
                ggplot2::scale_y_continuous(expand = expand,
                                            breaks = NULL,
                                            position = y.title.pos) +
                ggplot2::labs(x = x.title,
                              y = y.title,
                              title = title,
                              subtitle = subtitle,
                              caption = caption) +
                ggplot2::scale_fill_manual(values = cols))

    if (flip) eval(G, envir = environment()) + ggplot2::coord_flip()
    else eval(G, envir = environment())
#    G
#    return(invisible(X))
}
