#' Quadratic Function
#'
#' This function calculates the quadratic function for a given numeric vector.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector representing the result of the quadratic function.
#'
#' @examples
#' x <- 1:10
#' myquad(x)
#'
#' @seealso
#' Use \code{\link{plot}} for visualizing the quadratic function.
#'
#' @keywords internal
#' @export
myquad <- function(x){
  x^2 - 5*x + 6
}
