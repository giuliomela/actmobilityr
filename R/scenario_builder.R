#' Define a commuting mode change scenario
#'
#' This function creates a tibble with all relevant information for a mobility change
#' scenario for any Italian city and for several transportation means.
#'
#' @param city A `character` string.: the city for which the scenario needs to be built. The user must
#'   insert a valid name (starting with a capital letter) of any of the Italian municipalities.
#'   For the full list of available municipalities see `mun_codes`. No multiple names allowed.
#' @param mode_from A `character` string.: the transportation mode used by commuters before the
#'   commuting mode change. For the full list of modes available see `r knitr::combine_words(unique(comm_matrix_cities_km$mean_of_transp))`
#' @param mode_to  A `character` string : the active transportation mode to which individuals shift. The
#'   user cn choose from: `walk`, `bike` and `ebike`. No multiple names allowed.
#' @param mode_change_share A numeric value: the share of total commuters shifting from passive to active mobility. Value
#'   must be between `0` and `1`. Alternatively to a single value, also a vector of values can be provided,
#'   to specify a different value for each age group in `demo_data`. Age groups range from
#'   `20-24` to `60-64` years. Therefore the first value of the vector is the share of the `20-24` years  of age
#'   group, while the last refers to the `60-64` group. Each value must be between `0` and `1`. Vector length
#'   must be the same of that of the age groups considered.
#' @param age_specific A logical value: use `FALSE` to assume that the share of total commuters.
#'   who shift to active mobility is the same across age groups. Use `TRUE` to provide age-specific values
#'   (see `mode_change_share`).
#' @param comm_days A numeric value. Indicates the number of commuting days per week. Values must be between `1`
#'   and `5`.
#' @param max_km A numeri value. The distance threshold (km) below which a given share of commuters (defined with
#'   `mode_change_share`) shift from passive to active mobility. This distance is a one-way distance.
#'   For example, when `max_km = 5`, a given share of all commuting travels below (or equal) to 5 km one-way
#'   are performed with active modes of travel instead than passive.
#' @return A data.frame with the commuting mode change scenario, to be used to estimate health benefits.
#' @examples
#' scenario_builder("Palermo", "private_car_driver", "bike", 0.2, 4, 5)
#'
#' ## creating a vector of mode change shares
#'
#' shares <- c(0.4, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2, 0.1, 0.1)
#'
#' scenario_builder("Palermo", "private_car_driver", "bike", shares, 4, 5)
#'
scenario_builder <- function(city, mode_from, mode_to, mode_change_share, comm_days, max_km){

  if (length(city) > 1| length(mode_from) > 1 | length(comm_days) > 1 |
      length(mode_to) > 1) stop("Scenarios can be developed only for one city and transportation mode at once.")

  if (!is.element(city, comm_matrix_cities_km$municipality)) stop("Please provide a valida municipality name.
                                                                  Name must start with a capital letter and
                                                                  be in Italian.")

  if (!is.element(mode_from, unique(comm_matrix_cities_km$mean_of_transp))) stop("Please provide a valid
                                                                                 transport mode name.")

  if (!is.element(length(mode_change_share), c(1, length(unique(demo_data$age))))) stop("The mode_change_share
                                                                                       parameter can ben of lenght 1
                                                                                       or the same length as the
                                                                                       number of age groups
                                                                                       considered.")

  if (sum(mode_change_share > 1 | mode_change_share < 0) != 0) stop("The parameter 'mode_change_share'
                                                                    must be between 0 and 1")

  if(!is.element(comm_days, 1:5)) stop("Commuting days must be betwee 1 and 5.")

  # subsetting the commodity matrix

  comm_data <- subset(comm_matrix_cities_km,
                      municipality %in% city &
                        mean_of_transp %in% mode_from &
                        km_one_way <= max_km)

  # defining nuts codes of the administrative regions the selected city belongs to

  nuts2 <- unique(comm_data$nuts2)

  nuts3 <- unique(comm_data$nuts3)

  # applying the demographic structure of the province to the commuting individuals in the chosen city,
  # average of the last 5 years

  pop_shares <- demo_data[demodata$geo == nuts3, c("age", "pop_share")]

  pop_shares <- lapply(split(pop_shares, pop_shares$age),
         function(x) mean(x$pop_share, na.rm = T)
  )

  pop_shares <- data.frame(age = names(pop_shares), pop_shares = unlist(pop_shares)) # avg pop shares

  rownames(pop_shares) <- NULL

  # adding mode change shares (which might be age-specific)

  pop_shares$mode_change_share <- mode_change_share

  # building a dataframe to break down the number of mode-changing individuals into age groups

  scenario <- pop_shares[rep(1:nrow(pop_shares), each = nrow(comm_data)), ]

  scenario$mode_from <- mode_from

  scenario$mode_to <- mode_to

  # calculating number of individuals changing their commuting mode

  scenario$individuals <- pop_shares$pop_shares *
    rep(comm_data$individuals, length(pop_shares$age)) *
    pop_shares$mode_change_share

  scenario$daily_km <- rep(comm_data$km_round_trip, length(pop_shares$age))

  # Computing commuting times

  scenario$avg_speed <- speeds_met[speeds_met$mode == mode_to, ]$speed

  scenario$met <- speeds_met[speeds_met$mode == mode_to, ]$met

  scenario <- transform(scenario, weekly_travel_time = daily_km / avg_speed * comm_days)

  scenario

}
