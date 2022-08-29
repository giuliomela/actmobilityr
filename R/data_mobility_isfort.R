#' Mobility data provided by Isfort
#'
#' A dataset containing some mobility data collected by Isfort, an Italian
#' research centre on the transport sector. Such data are useful for the estimation
#' of changes in injury risk among new active mobility adopter. Information on deaths
#' are from ISTAT.
#'
#' @format A data frame with five variables.
#' \describe{
#'     \item{year}{Year}
#'     \item{mode}{Active mobility mode}
#'     \item{deaths}{Annual number of fatalities caused by road accidents}
#'     \item{modal-share}{Share of total trips done in a given mode}
#'     \item{avg-trip-length}{Average trip length}
#' }
#' @source \url{https://www.isfort.it/}
"data_mobility_isfort"
