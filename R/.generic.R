clip <- function(x, min, max) {
    x = as.matrix(x)
    x[x < min] <- min
    x[x > max] <- max
    x
}

cluster_by_identity <- function(dat, x = NULL, y = NULL, x.ord = F, y.ord = F) {
    dat <- as.data.frame(dat)
#    if (any(sapply(dat, class) == 'factor')) {
#        stop('Remove factor level(s)')
#    }
#
    if (is.null(x)) x = colnames(dat)[1]
    if (is.null(y)) y = colnames(dat)[2]

    form <- as.formula(paste(x,"~",y))
    m <- reshape2::acast(dat, form)
    m[is.na(m)] <- 0

    xord.call <- quote(clusta(t(m), ORD = T))
    yord.call <- quote(clusta(m, ORD = T))

    if (isTRUE(x.ord)) {
        return(list(m = m, ord = eval(xord.call)))
    }

    if (isTRUE(y.ord)) {
        return(list(m = m, ord = eval(yord.call)))
    }

    else {
        return(list(m = m, x.ord = eval(xord.call), y.ord = eval(yord.call)))
    }
}

as.bulk <- function(mat) {
    # starting from log2(TPM/10 + 1)
    mat = as.matrix(mat)
    mat = 10*(2^(mat)-1)
    mat = rowMeans(mat)
    log2(mat + 1)
}


colours.scale = function(breaks = 10,
                         cols = 'brwhite',
                         solarized = T,
                         shuffle = F,
                         ...) {

    if (isTRUE(solarized)) {
        sol.cols = solarized.colours(accent = accent, ...)
        if (!all(cols %in% names(sol.cols))) {
            warning('did not find cols in solarized colours, reverting to base')
        } else {
            cols = sol.cols[which(names(sol.cols) %in% cols)]
        }
    }
    #Create a function to generate a continuous color palette
    colours = sapply(2:length(cols), function(i) {
                         rbPal = colorRampPalette(cols[c(1, i)])
                         rbPal(breaks)}
    )

    colours = unique(colours)
    if (shuffle) {
        return(sample(colours))
    }

    colours
}

chr.len <- function(genes) {
    chr.order = c(paste0('chr', 1:9), paste0('chr1', 0:9), paste0('chr2', 0:2), 'chrX', 'chrY')
    dat = readRDS('gene.pos.rds') %>%
        dplyr::mutate(Gene = as.character(Gene)) %>%
        dplyr::filter(Gene %in% genes) %>%
        dplyr::distinct(Gene, Chromosome)

    chr.lens = split(dat$Gene, dat$Chromosome) %>% lengths
    cumsum(chr.lens[chr.order])
}


factorGroups = function(groups, levels = NULL, ord = NULL) {
   factors = rep(names(groups), lengths(groups))
    if (!is.null(ord)) {
        factors = factors[match(ord, unlist(groups))]
    }
    if (is.null(levels)) {
        levels = names(groups)
    }
    if (is.numeric(levels)) {
        levels = names(groups)[levels]
    }
    factor(factors, levels = levels)
}



arrangeByChr = function(genes) {
    chrLevels = c(paste0('chr', 1:9), paste0('chr1', 0:9), paste0('chr2', 0:2), 'chrX', 'chrY')
    chrdat = readRDS('gene.pos.rds') %>%
        dplyr::filter(Gene %in% genes) %>%
        dplyr::distinct(Gene, Chromosome) %>%
        dplyr::mutate(Chromosome = factor(as.character(Chromosome), levels = chrLevels)) %>%
        dplyr::arrange(Chromosome)
    setNames(chrdat$Gene, chrdat$Chromosome)
}

splitree = function(hc, ...) {
    cuts = cutree(hc, ...)
    split(names(cuts), cuts)
}


# https://stackoverflow.com/questions/13112238/a-matrix-version-of-cor-test
cor.test.p <- function(x){
    FUN <- function(x, y) cor.test(x, y)[["p.value"]]
    z <- outer(
      colnames(x), 
      colnames(x), 
      Vectorize(function(i,j) FUN(x[,i], x[,j]))
    )
    dimnames(z) <- list(colnames(x), colnames(x))
    z
}


cutmatrix = function(m, k = 2, m2 = NULL, Names = F) {
    # add height option (e.g. h = 1, or, to cut at all heights, h = hc$height)G
    hc = hclust(dist(1 - cor(m, method = 'pearson')), method = 'average')
    Nams = splitree(hc, k = k)
    if (Names) return(Nams)
    if (!is.null(m2)) m = m2
    sapply(Nams, function(x) m[, colnames(m) %in% x], simplify = F)
}


logfc = function(a, b) {
    setNames(rowMeans(as.matrix(a)) - rowMeans(as.matrix(b)), rownames(a))
}

t.test.rowwise = function(m1, m2, adjustMethod = 'bonferroni') {
    pvalues = sapply(1:nrow(m1), function(i) t.test(m1[i, ], m2[i, ])$p.value, simplify = F)
    setNames(p.adjust(pvalues, method = adjustMethod), rownames(m1))
}

t.tests.eval = function(m1,
                        m2,
                        cutoff = 10^(-3),
                        adjustMethod = 'bonferroni',
                        size = 0.3,
                        method = 'mean') {

    bool = t.test.rowwise(m1, m2) <= cutoff
    if (method == 'mean') {
        return(mean(bool) >= size)
    }

    return(sum(bool) >= size)
}

as.ranks = function(mat, colwise = T, ties.method = 'average') {
    mat = as.matrix(mat)
    rank.mat = mat
    if (colwise) {
        rank.mat[] <- apply(mat, 2, rank)
    } else {
        rank.mat[] <- rank(mat)
    }
    rank.mat
}
