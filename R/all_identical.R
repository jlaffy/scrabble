
# credit to flodel for the elegant solution
# stackoverflow.com/questions/18813526/check-whether-all-elements-of-a-list-are-in-equal-in-r

#' @title All Elements Of A List Are Identical
#' @description Check if all elements of a list are identical. This means both that the elements are the same in content, and the same in order.
#' @param l a list of elements
#' @return a boolean value
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  test_F = replicate(5, sample(colnames(mtcars)), simplify = F)
#'  test_T = rep(colnames(mtcars), 5, simplify = F)
#'  all_identical(l = test_F)
#'  all_identical(l = test_T)
#'  }
#' }
#' @rdname all_identical
#' @export 
all_identical = function(l) all(mapply(identical, head(l, 1), tail(l, -1)))

