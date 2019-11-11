
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[matrixStats]{rowCounts}}
#'  \code{\link[stats]{setNames}}
#' @rdname count_detected
#' @export 
#' @importFrom matrixStats colCounts
#' @importFrom stats setNames
count_detected = function(m) {
    m = as.matrix(m)
    res = matrixStats::colCounts(m != 0)
    stats::setNames(res, colnames(m))
}
