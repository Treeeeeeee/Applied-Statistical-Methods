#' Title
#'
#' @param x0 Initial guess for the parameter of interest.
#' @param delta Small increment used to approximate the derivative.
#' @param llik Function computing the log-likelihood.
#' @param xrange Range to be considered for plotting (not currently used in the function).
#' @param parameter Name of the parameter to be estimated.
#'
#' @return List containing the estimated parameter value and the number of iterations.
#' @export
#'
#' @examples
#' # Set up a simple log-likelihood function
#' llik <- function(lambda) { sum(dpois(x=4:10, lambda=lambda, log=TRUE)) }
#' # Find the maximum likelihood estimate for lambda starting from lambda=1
#' myNRML(x0=1, llik=llik, xrange=c(0,10), parameter="lambda")
#' @importFrom graphics plot
#' @importFrom graphics points
myNRML <- function(x0, delta=0.001, llik, xrange, parameter="param") {
  f <- function(x) (llik(x+delta) - llik(x)) / delta
  fdash <- function(x) (f(x+delta) - f(x)) / delta
  d <- 1000
  i <- 0
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])
  while(d > delta & i < 100) {
    i <- i + 1
    x[i+1] <- x[i] - f(x[i]) / fdash(x[i])
    y[i+1] <- f(x[i+1])
    d <- abs(y[i+1])
  }
  plot(x, type="l", main=paste("Convergence Plot for", parameter), xlab="Iteration", ylab="Lambda")
  points(x, col="red")
  return(list(lambda_hat=x[length(x)], iterations=i))
}
