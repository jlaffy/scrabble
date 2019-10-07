
.DEgenes.FCstep = function(m, m2, group, FC, is.log, returnAllGenes, returnVal) {

    returnVal = ifelse(length(returnVal) > 1, returnVal[[1]], returnVal)

    fcs = fold_changes(m = m,m2 = m2,group = group,is.log = is.log)

    # if data in log form, convert FoldChange value to log2
    if (is.numeric(FC) & is.log) {
        FC = log2(FC)
    }

    if (is.numeric(FC) & isFALSE(returnAllGenes) & returnVal != 'all') {
        genesPassingFC = which(fcs >= FC)
        fcs = fcs[genesPassingFC]
        m = m[genesPassingFC, , drop = F]
        m2 = m2[genesPassingFC, , drop = F]
    }
    
    list(fcs = fcs, genesPassingFC = genesPassingFC, m = m, m2 = m2)
}

.DEgenes.Pstep = function(m, m2, group, adjust.method) {
    ps = t_tests(m = m,
                 m2 = m2,
                 group = group,
                 adjust.method = adjust.method)
    list(ps)
}

.DEgenes.Sort = function(fcs, ps, sortBy) {

    sortBy = ifelse(length(sortBy) > 1, sortBy[[1]], sortBy)
    # allow plural spelling of methods (e.g. fcs instead of fc)
    sortBy = stringr::str_replace_all(sortBy, "s", "")

    if (sort %in% c('fc', 'foldchange', 'FC')) {
        Order = order(fcs, decreasing = T) # sort by fold_changes (largest first)
    }

    # sort by p values instead of by fold_changes
    else if (sort %in% c('p', 'pval', 'P', 'pvalue')) {
        Order = order(ps, decreasing = F) # sort by p values (smallest first)
    }
    
    # don't sort
    else if (sortBy == 'none') {
        Order = 1:length(fcs)
    }
    
    else stop("sortBy method '", sortBy, "' does not exist")
    
    list(fcs = fcs[Order], ps = ps[Order])
}

.DEgenes.Significant = function(p, FC, ps, fcs) {
    if (is.numeric(p) & is.numeric(FC)) {
        ind = which(fcs >= FC & ps <= p)
    } else if (is.numeric(p)) {
        ind = which(ps <= p)
    } else if (is.numeric(FC)) {
        ind = which(fcs >= FC)
    } else {
        ind = stats::setNames(1:length(fcs), names(fcs))
    }

    list(ind = ind)
}

.DEgenes.PrepareOutput = function(fcs, ps, ind, returnVal) {

    returnVal = ifelse(length(resultVal) > 1, returnVal[[1]], returnVal)

    if (returnVal == 'all') {
        result = list(fold.change = fcs, p.value = ps, DEgenes = ind))
    }

    else if (returnVal == 'p') {
        result = stats::setNames(ps[ind], names(fcs)[ind]) # return p values
    }
        
    else {
        result = stats::setNames(fcs[ind], names(fcs)[ind]) # return fold_changes
    }

    list(result = result)
}
