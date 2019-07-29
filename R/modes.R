
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION, Default: NULL
#' @param X PARAM_DESCRIPTION, Default: NULL
#' @param prob PARAM_DESCRIPTION, Default: 0.95
#' @param coverage PARAM_DESCRIPTION, Default: 0.8
#' @param size PARAM_DESCRIPTION, Default: 10
#' @param exclude.unassigned PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[mixtools]{normalmixEM}}
#' @rdname modes
#' @export 
#' @importFrom mixtools normalmixEM
modes = function(m = NULL,
                 X = NULL,
                 prob = 0.95,
                 coverage = 0.8,
                 size = 10,
                 exclude.unassigned = T) {

    # prob = min posterior probability
    # coverage = fraction with min posterior probability
    # size = min size of mode
    if (is.null(X)) {
        X = colMeans(m)
    }

    Xnames = names(X)
    if (is.null(Xnames)) Xnames = 1:length(X)
    
    probs = mixtools::normalmixEM(X)$posterior
    probs = probs %>%
        as.data.frame %>%
        mutate(bool.1 = comp.1 >= prob,
               bool.2 = comp.2 >= prob,
               bool = bool.1 | bool.2)
    
    ModesExist = mean(probs$bool) > coverage & sum(probs$bool.1) >= size & sum(probs$bool.2) >= size
    
    if (!ModesExist) {
        if (exclude.unassigned) {
            return(NULL)
        }
        return(setNames(colnames(m), rep(0, ncol(m))))
    }
    
    probs = probs %>%
        mutate(Group = ifelse(comp.1 >= prob, 1, ifelse(comp.2 >= prob, 2, 0)))
    
    Modes = setNames(Xnames, probs$Group)
    
    if (exclude.unassigned) {
        Modes = Modes[names(Modes) != '0']
    }

    Modes
}

