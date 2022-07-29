#' List of Italian municipalities
#'
#' A dataset containing official names of Italian municipalities and their ISTAT codes
#'
#' @format A tibble with 4 variables.
#' \describe{
#'  \item{code}{ISTAT codes}
#'  \item{name}{Municipality name in Italian}
#'  \item{region}{Region name in Italian}
#'  \item{province}{Province name in Italian}
#'  \item{nuts1}{Code of the NUTS1 to which the municipality belongs}
#'  \item{nuts2}{Code of the NUTS2 to which the municipality belongs}
#'  \item{nuts3}{Code of the NUTS3 to which the municipality belongs}
#' }
#' @source \url{https://www.istat.it/it/archivio/6789}
"mun_codes"
