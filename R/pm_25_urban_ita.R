#' Mean PM2.5 background concentrations in Italian cities
#'
#' A dataset of mean annual background concentrations of PM2.5 in some Italian
#' cities. Data are from the WHO Air quality Database 2022 and refer to the period
#' 2010 - 2019. The mean is computed over this period. Not all Italian municipalities
#' are in the database.
#'
#' @format A tibble with eight variables.
#' \describe{
#'     \item{name}{City name}
#'     \item{bkg_conc}{PM2.5 mean background concentration, ug/m~3~}
#'     \item{code}{Municipality ISTAT code}
#'     \item{region}{Region name}
#'     \item{province}{Province name}
#'     \item{nuts1}{NUTS1 code}
#'     \item{nuts2}{NUTS2 code}
#'     \item{nuts3}{NUTS3 code}
#'     }
#' @source \url{https://www.who.int/publications/m/item/who-air-quality-database-2022}
"pm_25_urban_ita"
