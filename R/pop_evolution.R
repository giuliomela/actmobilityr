#' Calculate the evolution of a population given a mortality rate
#'
#' This function calculates the year-by-year evolution of a population given the
#' yearly mortality rate. It is useful to compute avoided and additional deaths
#' in mobility change scenarios. The input for year _n_ is the output of year _n-1_.
#'
#' @param pop A numeric value. The initial number of individuals.
#' @param death_rate A numeric vector. A vector of annual mortality rates. A vector
#'    is used to allow the user to apply different mortality rates for different years.
#' @return A vector of population values of the same length of `death_rate`.
#' @examples
#' pop_evolution(500, seq(from = 0.001, to = 0.0006, by = - 0.0001))
pop_evolution <- function(pop, death_rate) {

  purrr::accumulate(death_rate[2:length(death_rate)], function (x, y) x * (1 - y),
  .init = pop * (1 - death_rate[1]))

}
