
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cormat PARAM_DESCRIPTION
#' @param lim.find PARAM_DESCRIPTION, Default: T
#' @param limits PARAM_DESCRIPTION, Default: c(-0.2, 0.2)
#' @param x.name PARAM_DESCRIPTION, Default: 'Cells'
#' @param y.name PARAM_DESCRIPTION, Default: 'Cells'
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @param cols PARAM_DESCRIPTION, Default: heatCols
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[reshape2]{melt}}
#' @rdname corPlot
#' @export 
#' @importFrom reshape2 melt
corPlot = function(cormat,
                   lim.find = T,
                   limits = c(-0.2, 0.2),
                   x.name = 'Cells',
                   y.name = 'Cells',
                   title = NULL,
                   subtitle = NULL,
                   cols = heatCols,
                   ...) {

    dat = reshape2::melt(as.matrix(cormat))
    .gmap(dat, x = Var1, y = Var2, fill = value,
          lim.find = lim.find, limits = limits, ratio = 1,
          y.name = y.name, x.name = x.name,
          title = title, subtitle = subtitle, legend.title = 'Cor.', ...)
}
