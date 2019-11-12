
annomap = function(X,
                   title = NULL,
                   x.title = NULL, 
                   pal = 1,
                   angle = 0,
                   breaks = ggplot2::waiver(),
                   x.num = T,
                   flip = F,
                   hide.legend = T,
                   legend.pos = 'top',
                   ratio = 0.03,
                   mar = 0.015,
                   cols = NULL,
                   cols.order = names(cols)) {

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
    y.title.pos = 'left'
    x.title.pos = 'bottom'
    # adjust for flipped plot
    if (flip) {
        ratio = 1/ratio
        y.title.pos = 'right'
        x.title.pos = 'bottom'
    }

    # ggplot x scale
    if (x.num) {
        G = ggplot2::ggplot(X, aes(x = as.numeric(x), fill = fill, y = 1))
        scale_x_choose = ggplot2::scale_x_continuous
    }
    else {
        G = ggplot2::ggplot(X, aes(x = x, fill = fill, y = 1))
        scale_x_choose = ggplot2::scale_x_discrete
    }

    # plot
    G = G + ggplot2::geom_tile() + 
            ggplot2::theme_bw() +
            scale_x_choose(expand = expand,
                           position = x.title.pos,
                           breaks = breaks,
                           name = x.title) +
            ggplot2::scale_y_continuous(name = title,
                                        expand = expand,
                                        breaks = NULL,
                                        position = y.title.pos) +
            ggplot2::theme(aspect.ratio = ratio,
                           legend.position = legend.pos,
                           axis.title.x = ggplot2::element_text(angle = angle), 
                           axis.title.y = ggplot2::element_text(angle = angle),
                           plot.margin = margins(mar, mar, mar, mar, "cm")) + 
            ggplot2::scale_fill_brewer(palette = pal, type = 'qual')

    if (!is.null(cols.order)) {
        col.groups = unique(X$fill)
        if (is.null(cols)) cols = rainbow(n = length(col.groups))
        cols = cols[match(col.groups, cols.order)]
        G = G + ggplot2::scale_fill_manual(values = cols)
    }

    if (flip) G = G + ggplot2::coord_flip()
    legend = ggpubr::get_legend(G)
    if (hide.legend) G = G + ggpubr::rremove("legend")
#    plot(G)
#    invisible(list(plot = G, data = X, legend = legend))

    G
}

