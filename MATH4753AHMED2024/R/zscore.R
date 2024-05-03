#' Z score from a vector
#' Compute Z score for outlier detection
#' Input parameter must be a vector
#'
#' @param x a vector of interest
#'
#' @importFrom stats sd
#' @return z score of a vector
#' @examples zscore(x)
#' @export
zscore <- function(x){
  z <- (x - mean(x))/sd(x)
  return(z)
}
