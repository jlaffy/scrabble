
#' @title Uniform Manifold Approximation and Projection
#' @description Computes a manifold approximation and projection and returns the embedding coordinates matrix. Wrapper function for umap::umap().
#' @param mat input matrix (each row is a variable / a gene and each column is an observation / a cell. 
#' @param dist.method a character string for the distance metric used. One of pearson, euclidean, manhattan, cosine or pearson2. Default: 'pearson'
#' @param ... other arguments passed to umap
#' @return coordinates embedding matrix
#' @seealso 
#'  \code{\link[umap]{umap.defaults}},\code{\link[umap]{c("umap", "umap")}}
#' @rdname build_umap
#' @export 
#' @importFrom umap umap.defaults umap
build_umap = function(mat, dist.method = 'pearson', ...) {
    # min.dist 0.001 ?
    umap.custom = umap::umap.defaults
    umap.custom$metric = dist.method
    coord.mat = umap::umap(t(mat), config = umap.custom, ...)$layout
    colnames(coord.mat) = c('UMAP.Dim1', 'UMAP.Dim2')
    coord.mat
}


#' @title t-Distributed Stochastic Neighbour Embedding (Barnes-Hut implementation)
#' @description Computes a low dimensional embedding of high-dimensional data and returns the embedding coordinates matrix. Wrapper function for Rtsne::Rtsne().
#' @param mat input matrix (each row is a variable / a gene and each column is an observation / a cell. 
#' @param ... other arguments passed to Rtsne
#' @return coordinates embedding matrix 
#' @seealso 
#'  \code{\link[Rtsne]{Rtsne}}
#' @rdname build_tsne
#' @export 
#' @importFrom Rtsne Rtsne
build_tsne = function(mat, ...) {
    coord.mat = Rtsne::Rtsne(t(mat), ...)$Y
    rownames(coord.mat) = colnames(mat)
    colnames(coord.mat) = c('tSNE.Dim1', 'tSNE.Dim2')
    coord.mat
}

