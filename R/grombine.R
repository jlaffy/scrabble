
.is_grob <- function(x) {
    'grob' %in% class(x)
} 

.which_to_grob <- function(x) {
    sapply(x, function(i) !is_grob(i))
}

.add_rows <- function(Grob, n) {
    if (n < 1) {return(Grob)}
    for (i in 1:n) {
        Grob <- gtable::gtable_add_rows(Grob, unit(0, "null"))
    }

    Grob
}


.add_cols <- function(Grob, n) {
    if (n < 1) {return(Grob)}
    for (i in 1:n) {
        Grob <- gtable::gtable_add_cols(Grob, unit(0, "null"))
    }
    Grob
}


.equalize_grob_dim <- function(grobs, FUN.count = ncol, FUN.add = .add_cols) {
    Dims <- sapply(grobs, FUN.count) %>% unlist(use.names = F)
    dimmax <- max(Dims)
    dimdifs <- dimmax - Dims
    Map(FUN.add, Grob = grobs, n = dimdifs)
}


grombine <- function(..., by = 'x', size = 'last', draw = T) {
    if (by %in% c('y', 'column')) {
        FUN.count = nrow
        FUN.add = .add_rows
        FUN = cbind
    }

    else if (by %in% c('ro', 'x')) {
        FUN.count = ncol
        FUN.add = .add_cols
        FUN = rbind
    }

    dots <- list(...)
    grobs <- sapply(dots, function(obj) ifelse(.is_grob(obj), return(obj), return(ggplot2::ggplotGrob(obj))), simplify = F)
    grobs <- .equalize_grob_dim(grobs, FUN.count = FUN.count, FUN.add = FUN.add)
    args <- c(grobs, list(size = size))
    Grob <- do.call(what = FUN, args = args)
    if (!draw) return(Grob)
    grid::grid.draw(Grob)
}



.marrangegrob <- function(...,
                          by = 'x',
                          size = 'last',
                          draw = FALSE,
                          nrow = 2,
                          ncol = 2,
                          marrange = F) {

    dots <- list(...)
    grobs <- sapply(dots, function(obj) {
                        ifelse(is_grob(obj), return(obj), return(ggplotGrob(obj)))}
        , simplify = F)

    Grob = marrangeGrob(grobs = grobs, nrow = nrow, ncol = ncol)
    if (!draw) return(Grob)
    grid::grid.draw(Grob)
}
