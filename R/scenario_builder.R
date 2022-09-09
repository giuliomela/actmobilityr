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
#' @param mode_change_distance A numeric vector: the shares of total commuters shifting from passive to active mobility. Single values
#'   must be between `0` and `1`. The vector must have `length = 4`. The first value refers to trips shorter than
#'   3 km (one way). The second to trips between 3 and 10 km, the third to trips between  10 and 15 km and the
#'   fourth to trips longer than 15 km. No default value is provided. Alternatively a single value - common to all
#'   distances - can be provided.
#' @param mode_change_age A numeric vector. Since people of different ages might be more or less prone to changing
#'   their commuting habits, the user can also decide to scale down or up the shares defined with `mode_change_distance`
#'   providing a vector of length `9` of values, one for each age group (from `20-24` to `60-64` years). Values can
#'   range between `0` and `1 - mode_change_distance` for any distance range. For example, if `mode_change_distance = 0.2` for trips between 3 and 10 km, the user
#'   can modify such percentage for some age groups. If the first element of `mode_change_age` is set to `0.1`, it
#'   means that the share of people aged `20-24` years that change their commuting habits is `0.2 + 0.1 = 0.3`, that
#'   is the "baseline" share set with `mode_change_distance` is increased by 10%. Default is set to `0` for all
#'   age groups.
#' @param age_specific A logical value: use `FALSE` to assume that the share of total commuters.
#'   who shift to active mobility is the same across age groups. Use `TRUE` to provide age-specific values
#'   (see `mode_change_share`).
#' @param comm_days A numeric value. Indicates the number of commuting days per week. Values must be between `1`
#'   and `5`.
#' @param max_km A numeric value. The distance threshold (km) below which a given share of commuters (defined with
#'   `mode_change_share`) shift from passive to active mobility. This distance is a one-way distance.
#'   For example, when `max_km = 5`, a given share of all commuting travels below (or equal) to 5 km one-way
#'   are performed with active modes of travel instead than passive.
#' @param ref_yr A numeric value. The reference year of the analysis.
#' @param scenario_length A numeri value. The length of the scenario in years.
#' @return A data.frame with the commuting mode change scenario, to be used to estimate health benefits.
#' @export
#' @examples
#' scenario_builder("Palermo", "private_car_driver", "bike", 0.2, 4, 5)
#'
#' ## creating a vector of mode change shares
#'
#' mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)
#'
#' scenario_builder("Palermo", "private_car_driver", "bike", mode_change_distance = mode_change_shares, comm_days = 4, max_km = 5)
#'
scenario_builder <- function(city, mode_from, mode_to, mode_change_distance, mode_change_age = rep(0, 9),
                             comm_days, max_km,
                             ref_yr = 2020, scenario_length = 10){

  # checking for errors in input data and packages required

  rlang::check_installed(c("dplyr", "knitr"), reason = "to use `scenario_builder`")

  if (length(city) > 1| length(mode_from) > 1 | length(comm_days) > 1 |
      length(mode_to) > 1) stop("Scenarios can be developed only for one city and transportation mode at once.")

  if (!is.element(city, comm_matrix_cities_km$municipality)) stop("Please provide a valida municipality name.
                                                                  Name must start with a capital letter and
                                                                  be in Italian.")

  if (!is.element(mode_from, unique(comm_matrix_cities_km$mean_of_transp))) stop(paste0("Please provide a valid
                                                                                 transport mode name: ",
                                                                                        knitr::combine_words(unique(comm_matrix_cities_km$mean_of_transp), and = "or")))

  if (!is.element(length(mode_change_distance), c(1, 4))) stop("The mode_change_distance
                                                                                       parameter can ben of lenght 1
                                                                                       or the same length as the
                                                                                       number of age groups
                                                                                       considered.")
  if (!is.element(mode_to, c("bike", "ebike", "walk"))) stop ("Please provide a valid
                                                                transport mode name: bike, ebike or walk")


  if (any(mode_change_distance > 1) | any(mode_change_distance < 0)) stop("The parameter 'mode_change_distance'
                                                                    must be between 0 and 1")

  if (any(unlist(lapply(mode_change_age, function (x) mode_change_distance + x)) > 1)) stop ("The sum between
                                                                                        'mode_change_distance' and
                                                                                        'mode_change_age' must be
                                                                                        lower or equal to 1")


  if(!is.element(comm_days, 1:5)) stop("Commuting days must be betwee 1 and 5.")

  scenario_yr <- ref_yr + scenario_length - 1

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

  pop_shares <- demo_data[demo_data$geo == nuts3, c("age", "pop_share")]

  # adding age-specific mode change shares

  mode_change_age <- data.frame(age = sort(unique(demo_data$age)),
                                mode_change_age = mode_change_age)

  pop_shares <- merge(pop_shares, mode_change_age)

  # adding distance-specific mode change shares to the commuting data

  comm_data$mode_change_distance <- dplyr::case_when(
    comm_data$km_one_way < 3 ~ mode_change_distance[1],
    comm_data$km_one_way >= 3 & comm_data$km_one_way < 10 ~ mode_change_distance[2],
    comm_data$km_one_way >= 10 & comm_data$km_one_way < 15 ~ mode_change_distance[3],
    TRUE ~ mode_change_distance[4]
    )

  comm_data_light <- comm_data[, c("individuals", "km_round_trip", "mode_change_distance")]

  # building a data frame to break down the number of mode-changing individuals into age groups

  scenario <- pop_shares[rep(1:nrow(pop_shares), each = nrow(comm_data)), ]

  comm_data_light <- comm_data_light[rep(1:nrow(comm_data_light), times = nrow(scenario)/nrow(comm_data_light)), ]

  rownames(comm_data_light) <- NULL

  scenario <- cbind(scenario, comm_data_light)

  rownames(scenario) <- NULL

  scenario$mode_from <- mode_from

  scenario$mode_to <- mode_to

  # calculating number of individuals changing their commuting mode (overwrites old individuals variable)

  scenario$individuals <- scenario$individuals *
    (scenario$mode_change_age + scenario$mode_change_distance)

  names(scenario)[names(scenario) == 'km_round_trip'] <- 'daily_km'

  # Computing commuting times

  scenario$avg_speed <- speeds_met[speeds_met$mode == mode_to, ]$speed

  scenario$met <- speeds_met[speeds_met$mode == mode_to, ]$met

  scenario <- transform(scenario, weekly_travel_time = daily_km / avg_speed * comm_days,
                        city = city)

  # adding rows corresponding to the years that make up the scenario

  scenario <- scenario[rep(1:nrow(scenario), each = scenario_length), ]

  # adding time variable

  scenario$year <- rep(ref_yr:scenario_yr, length(unique(scenario$age)) *
                         length(unique(scenario$daily_km)))

  scenario$individuals <- ifelse(scenario$year > ref_yr, NA_real_,
                                 scenario$individuals) # eliminates individuals data for future years

  # adding weekly commuting days

  scenario$weekly_comm_days <- comm_days

  scenario <- scenario[order(scenario$year, scenario$age, scenario$daily_km), ]

  scenario

}
