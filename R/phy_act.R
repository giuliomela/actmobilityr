#' Physical activity levels
#'
#' A dataset containing data on the physical activity levels among Italian citiziens.
#' Data are about the time spent on health-enhancing (non-work-related) aerobic physical activity,
#' by age group. Data refer to Italy as a whole and to the year 2019.
#' Age group in the original dataset are different from those used in this package. An adaptation was made.
#'
#' @format A tibble with three variables.
#' \describe{
#'  \item{duration}{Duration of physical activity, minutes/week}
#'  \item{age}{age group}
#'  \item{phy_act_share}{Share of the population spending time in health-enhancing physical activity}
#' }
#' @source \url{https://ec.europa.eu/eurostat/data/database}
"phy_act"
