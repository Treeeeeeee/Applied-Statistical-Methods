#' Calculate the number of tickets to be sold
#'
#' This function calculates the number of tickets to be sold for a flight given
#' the number of seats, the probability of a "show", and the probability of overbooking.
#' It provides two methods of calculation: one using a discrete distribution and
#' the other using a normal approximation.
#'
#' @param N Number of seats in the flight.
#' @param gamma Probability a plane will be truly overbooked (more people show than there are seats).
#' @param p Probability of a "show".
#'
#' @return A list containing nd, nc, N, p, and gamma.
#' @details The function calculates nd using the appropriate discrete distribution
#' and nc using the normal approximation. It then prints a named list containing
#' nd, nc, N, p, and gamma. Additionally, it creates plots of the objective function
#' versus n for both the discrete and continuous cases.
#' @examples
#' ntickets(N = 150, gamma = 0.1, p = 0.05)
#' @export
ntickets <- function(N, gamma, p) {
  # Calculate nd using discrete distribution
  nd <- stats::qpois(1 - gamma - p, lambda = N)

  # Calculate nc using normal approximation
  mu <- N * (1 - gamma - p)
  sigma <- sqrt(N * (1 - gamma - p) * (1 + gamma))
  nc <- stats::qnorm(1 - gamma, mean = mu, sd = sigma)

  # Print named list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Create plot of Objective function Vs n for discrete case
  n_values <- seq(0, N * 2, by = 1)
  objective_discrete <- 1 - (1 - gamma - stats::ppois(n_values, lambda = N))
  plot(n_values, objective_discrete, type = 'l', col = 'blue',
       xlab = 'n', ylab = 'Objective function', main = 'Objective function Vs n (Discrete)')

  # Create plot of Objective function Vs n for continuous case
  objective_continuous <- stats::pnorm(n_values, mean = mu, sd = sigma)
  plot(n_values, objective_continuous, type = 'l', col = 'red',
       xlab = 'n', ylab = 'Objective function', main = 'Objective function Vs n (Continuous)')
}


