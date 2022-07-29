#' Commuting matrix data of Italian municipalities
#'
#' A dataset containing commuting data regarding Italian municipalities. Data refer
#' to the year 2011 (most recent data available). For more info see the `source` tab.
#' Data refer to the whole population commuting for work (both males and females).
#'
#'@format A tibble with 14 variables
#'\describe{
#' \item{code}{Istat municipality code}
#' \item{municipality_res_name}{Municipality name}
#' \item{region}{Region label (Italian)}
#' \item{province}{Province label (Italian)}
#' \item{nuts1}{NUTS1 code}
#' \item{nuts2}{NUTS2 code}
#' \item{nuts3}{NUTS3 code}
#' \item{mean_of_transp}{Mean of transportation name}
#' \item{travel_time}{Travel time in minutes, categorical variable}
#' \item{individuals}{Number of individuals}
#' \item{avg_travel_time}{Travel time in minutes, continuous variable calculated using average speeds and the mid-point of travel time}
#' \item{speed_kmh}{Average speed of a given transpotation mean}
#' \item{km_one_way}{Average distance covered by commuters, one way, computed using average speed and commuting time}
#' \item{km_round_trip}{Average distance covered by commuters, round trip, computed using average speed and commuting time}
#'}
#'@source \url{https://www.istat.it/it/archivio/157423}
"comm_matrix_cities_km"
