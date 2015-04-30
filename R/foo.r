#' test
#' @param x x
#' @export
conv <- function(a, b) .Call(convolve2, a, b, PACKAGE = "lambdass")


#' @export
add <- function(x, y) .Call(C_add_, x, y)
