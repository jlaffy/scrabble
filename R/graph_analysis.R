
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat input matrix (each row is a variable / a gene and each column is an observation / a cell. 
#' @param method character string defining the dimension-reduction method used. One of 'umap' or 'tsne', calling umap::umap or Rtsne::Rtsne, respectively. Default: 'tsne'
#' @param ... other arguments passed to dimension-reduction method (umap or Rtsne)
#' @return coordinates embedding matrix
#' @rdname build_coordinates
#' @export 
build_coordinates = function(mat, method = 'tsne', ...) {
    if (method == 'tsne') {
        FUN = build_tsne
    } else if (method == 'umap') {
        FUN = build_umap
    } else {
        stop('Method should be one of tsne or umap.')
    }

    FUN(mat = mat, ...)
}


.knn_as_data_frame = function(knn, k, member_names = NULL) {
    from = rep(1:nrow(knn$nn.index), k)
    to = as.vector(knn$nn.index)
    weight = 1/(1 + as.vector(knn$nn.dist))
    dat = data.frame(from = from, to = to, weight = weight)
    
    if (!is.null(member_names)) {
        members = stats::setNames(1:nrow(mat), member_names)
        dat = dplyr::mutate(dat, from = names(members)[match(dat$from, members)])
        dat = dplyr::mutate(dat, to = names(members)[match(dat$to, members)])
    }

    dat
}


#' @title Search Nearest Neighbours
#' @description computes fast k-nearest neighbour search. Wrapper for FNN::get.knn().
#' @param mat coordinates embedding matrix (such as that generated with build_tsne() or build_umap())
#' @param k a numeric value or vector defining the maximum number or numbers of nearest neighours to search. If a vector is provided, the function will iterate for every value of k. Default: seq(5, 30, 1)
#' @param algorithm nearest neighbour searching algorithm. One of kd_tree, cover_tree, CR, brute. Default: 'kd_tree'
#' @return a dataframe with the k-nearest neighbours and corresponding weights (1/1 + the euclidean distance).
#' @seealso 
#'  \code{\link[FNN]{get.knn}}
#' @rdname build_knn
#' @export 
#' @importFrom FNN get.knn
build_knn = function(mat, k = seq(5, 30, 1), algorithm = 'kd_tree') {

    .build_knn = function(mat, k, algorithm) {
        mat = as.matrix(mat)
        knn = FNN::get.knn(mat, k = k, algorithm = algorithm)
        knn = .knn_as_data_frame(knn = knn, k = k, member_names = rownames(mat))
    }

    sapply(k, function(ki) {
               .build_knn(mat = mat, k = ki, algorithm = algorithm)},
               simplify = F)
}
 

#' @title Create igraph graph from KNN output
#' @description Computes an igraph graph from KNN output or a KNN data frame with three columns - 'from', 'to', and 'weight'.
#' @param knn a dataframe with the k-nearest neighbours and corresponding weights (1/1 + the euclidean distance).
#' @param member_names a character vector. Only applicable if KNN output is provided rather than a KNN dataframe, and this will replace the member IDs with the character names. Default: NULL
#'  \code{\link[igraph]{as_data_frame}},\code{\link[igraph]{simplify}}
#' @rdname build_graph
#' @export 
#' @importFrom igraph graph_from_data_frame simplify
build_graph = function(knn, member_names = NULL) {
    
    .build_graph = function(knn) {

        if (!is.data.frame(knn)) {
            knn = .knn_as_data_frame(knn, member_names = member_names)
        }

        network = igraph::graph_from_data_frame(knn, directed = F)
        igraph::simplify(network)
    }

    sapply(1:length(knn), function(i) .build_graph(knn = knn[[i]]), simplify = F)
}


#' @title Cluster igraph graphs
#' @description Compute and retrieve clusters from igraph graphs.
#' @param graph igraph graph object.
#' @param method a character string, one of 'louvain' or 'hierarchical', defining the clustering method used. Default: 'louvain'
#' @return a numeric vector of member IDs whose names are the members (or member IDs), or a list of numeric vectors if a list of igraph graphs was provided.
#' @seealso 
#'  \code{\link[igraph]{cluster_louvain}},\code{\link[igraph]{cluster_walktrap}},\code{\link[igraph]{membership}}
#' @rdname cluster_graph
#' @export 
#' @importFrom igraph multilevel.community walktrap.community membership
cluster_graph = function(graph, method = 'louvain') {

    .cluster_graph = function(graph, method = 'louvain') {
        if (method == 'louvain') {
            cluster_FUN = igraph::multilevel.community
        } else if (method == 'hierarchical') {
            cluster_FUN = igraph::walktrap.community
        } else {
            stop('Method should be one of louvain or hierarchical')
        }
        clustering = cluster_FUN(graph)
        igraph::membership(clustering)
    }

    sapply(1:length(graph), function(i) .cluster_graph(graph = graph[[i]], method = method), simplify = F)
}


#' @title A Graph-Based Clustering Framework
#' @description Graph-based clustering consisting of the following steps: (1) build a coordinates embedding matrix (e.g. with tSNE, UMAP, MDS). (2) Search for k-nearest neighbours and build a dataframe with resulting neighbours and their weights. (3) Build a graph from the k-nearest neighbours. (4) Cluster the graph with one of a number of methods, e.g. Louvain clustering. (5) Return the clusters. If any of the arguments corresponding to each of these steps is set to TRUE, the function will stop after the relevant computation and return the object set to TRUE. Alternatively if set to FALSE, the computation will continue, unless an earlier step was set to TRUE. If instead, for any of these objects, the relevant object was provided, this object will be used instead of computed. 
#' @param mat input matrix. Default: NULL
#' @param coord.mat coordinates embedding matrix (such as that generated with build_tsne() or build_umap())
#' @param knn a boolean value or an object corresponding to FNN::get.knn output or a knn a dataframe with the k-nearest neighbours and corresponding weights (1/1 + the euclidean distance). Default: F
#' @param graph igraph graph object. Default: F
#' @param clusters a numeric vector (or list of vectors) of cluster IDs with as vector names the corresponding members (or member IDs). Default: F
#' @param k a numeric value or vector defining the maximum number or numbers of nearest neighours to search. If a vector is provided, the function will iterate for every value of k. Default: seq(5, 30, 1)
#' @param coord.method character string defining the dimension-reduction method used. One of 'umap' or 'tsne', calling umap::umap or Rtsne::Rtsne, respectively. Default: 'tsne'
#' @param knn.method nearest neighbour searching algorithm. One of kd_tree, cover_tree, CR, brute. Default: 'kd_tree'
#' @param cluster.method a character string, one of 'louvain' or 'hierarchical', defining the clustering method used. Default: 'louvain'
#' @param ... other arguments passed to umap::umap or Rtsne::Rtsne.
#' @return an object if one of coord.mat, knn, graph or clusters is set to TRUE, otherwise a list containing each of the objects computed and the configuration values. If multiple k's are desired, then the knn, graph and clusters objects will each be lists of the same lengths as there are ks.
#' @rdname graph_analysis
#' @export 
graph_analysis = function(mat = NULL,
                          coord.mat = F,
                          knn = F,
                          graph = F,
                          clusters = F,
                          k = seq(5, 30, 1),
                          coord.method = 'tsne',
                          knn.method = 'kd_tree',
                          cluster.method = 'louvain',
                          ...) {

    objects_to_compute = list(coord.mat, knn, graph, clusters)
    start_computation = 0
    end_computation = 5
    custom_start = sapply(objects_to_compute, function(obj) !is.logical(obj))
    custom_end = sapply(objects_to_compute, isTRUE)

    if (any(custom_start)) {
        start_computation = max(which(custom_start))
    }

    if (any(custom_end)) {
        end_computation = max(which(custom_end))
    }

    if (start_computation == 0) {
        coord.mat = build_coordinates(mat = mat, method = coord.method, ...) 
        start_computation = start_computation + 1
    }

    if (end_computation == 1) {
        return(coord.mat)
    }

    if (start_computation == 1) {
        knn = build_knn(mat = coord.mat, k = k, algorithm = knn.method)
        start_computation = start_computation + 1
    }

    if (end_computation == 2) {
        return(knn)
    }

    if (start_computation == 2) {
        graph = build_graph(knn = knn)
        start_computation = start_computation + 1
    }

    if (end_computation == 3) {
        return(graph)
    }

    if (start_computation == 3) {
        clusters = cluster_graph(graph, method = cluster.method)
    }

    if (end_computation == 4) {
        return(clusters)
    }

    config = list('parameters!')

    list(coord.mat = coord.mat,
         knn = knn,
         graph = graph,
         clusters = clusters,
         config = config)
}

