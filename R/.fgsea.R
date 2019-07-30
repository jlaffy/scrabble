#!/usr/bin/env Rscript

library(fgsea)

# GET RELEVANT DATASETS FOR ENRICHMENT ANALYSIS
pathways <- function(species = "Homo sapiens",
                     cat = NULL ,
                     subcat = NULL,
                     by_subcat = T,
                     by_cat = F) {
    require(msigdbr)

    m_df = msigdbr(species = species) %>%
        dplyr::mutate(gs_subcat = ifelse(gs_subcat == '',
                                         as.character(gs_cat),
                                         as.character(gs_subcat)))

    dataframe_out <- quote(m_df %>% dplyr::select(gs_name, gene_symbol) %>% as.data.frame())

    if (!is.null(cat)) {
        m_df = m_df %>% dplyr::filter(gs_cat %in% cat)
    }

    if (!is.null(subcat)) {
        m_df = m_df %>% dplyr::filter(gs_subcat %in% subcat)
    }

    # List of gene sets nested by MSigDB categories AND subcategories
    if (isTRUE(by_cat) & isTRUE(by_subcat)) {
        m_dfs = split(m_df, m_df$gs_cat) %>%
            sapply(., function(d) split(d, d$gs_subcat), simplify = F)

        return(lapply(m_dfs, sapply, function(m_df) eval(dataframe_out), simplify = F))
    }

    # List of gene sets nested by MSigDB subcategories
    if (isTRUE(by_subcat)) {
        m_dfs = split(m_df, m_df$gs_subcat)
        return(sapply(m_dfs, function(m_df) eval(dataframe_out), simplify = F))
    }

    # List of gene sets nested by MSigDB categories 
    else if (isTRUE(by_cat)) {
        m_dfs = split(m_df, m_df$gs_cat)
        return(sapply(m_dfs, function(m_df) eval(dataframe_out), simplify = F))
    }


    # Flat list of gene sets
    eval(dataframe_out)
}



# RUN ENRICHMENT ANALYSIS
enrich <- function(gene,
                   universe,
                   sets = NULL,
                   cats = c('C2', 'C5'),
                   minGSSize = 5,
                   pAdjustMethod = 'bonferroni') {

    if (is.null(sets)) {
        sets = pathways(cat = cats, by_subcat=F)
    }

    result = clusterProfiler::enricher(gene = gene,
                                       TERM2GENE = sets,
                                       universe = universe,
                                       minGSSize = minGSSize,
                                       pAdjustMethod = pAdjustMethod)
    result
}
