#' Ventilation data of the main activities carried out by individuals during the day
#'
#' A data frame containing data on ventilation rates and conversion factors, needed to
#' estimate the actual air pollutant concentration in different locations starting from
#' background concentration data.
#'
#' @format a tibble with three variables
#' \describe{
#'     \item{activity}{The type of activity carried out by individuals}
#'     \item{vent_rates}{Ventilation rate, m~3~/h}
#'     \item{con_fct}{Conversion factors, to derive actual concentrations from background's}
#' }
#' @source Conversion factors are from the HEAT manual. Ventilation rates are from
#'     different sources.
"ventilation_data"
