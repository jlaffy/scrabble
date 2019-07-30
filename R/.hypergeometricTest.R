
Phyper = function(s, S, n, N, log.p = F, lower.tail = F) {
    # if test for depletion, lower.tail = T
    # if test for enrichment, lower.tail = T
    
    # note: if test for enrichment, lower.tail = T and [X > x]
    # so make s = s - 1 such that [X >= x]
    if (isFALSE(lower.tail)) {
        s = s - 1
    }

    stats::phyper(s, n, N - n, S, log.p = log.p, lower.tail = F)
}


ndat = function(sets) {
    # successes in population sizes
    melt(lengths(sets)) %>%
        tibble::rownames_to_column('group') %>%
        rename(n = value)
}

sdat = function(sets, groups) {
    # successes in samples sizes
    sapply(sets, function(set) {
               sapply(groups, function(group) {
                          sum(set %in% group)},
                          simplify = F)},
           simplify = F) %>%
    melt %>%
    rename(s = value,
           cellType = L2,
           group = L1)
}

Sdat = function(groups) {
    # sample sizes
    melt(lengths(groups)) %>%
        tibble::rownames_to_column('cellType') %>%
        rename(S = value)
}

enrichment = function(ingroups,
                      ofsets,
                      universe = NULL,
                      lower.tail = F,
                      return.full = F,
                      adjust.method = 'BH') {

    # no test for multiple correction is performed if adjust.method = 'none'
    if (is.null(universe)) {
        universe = unlist(ingroups) %>% unique
    }

    dat = left_join(sdat(sets = ofsets, groups = ingroups),
                    ndat(sets = ofsets),
                    by = c('group'))
    
    dat = left_join(dat,
                    Sdat(groups = ingroups),
                    by = c('cellType'))

    dat = dat %>%
        mutate(N = rep(length(universe), nrow(dat))) %>%
        select(group, cellType, s, S, n, N)


    dat$p = Map(Phyper,
                s = dat$s,
                S = dat$S,
                n = dat$n,
                N = dat$N,
                MoreArgs = list(lower.tail = lower.tail)) %>% as.numeric

    dat$Enrichment = -log10(dat$p)

    if (return.full) return(dat)
    dat %>% select(group, cellType, Enrichment)
}
