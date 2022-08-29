pollution_impact <- function (scenario) {

# identifying background concentrations of the city considered

  # Identifying city name from scenario data

  city_name <- scenario$city[1]

  if (city_name %in% pm_25_urban_ita$name) {

    # if the municipality is in the WHO database, the actual value is used

    bkg_conc <- pm_25_urban_ita[pm_25_urban_ita$name == city_name, ]$bkg_conc

  } else {

    # otherwise, the average background concentration of all cities in the province to which
    # the municipality considered belongs is used.

    nuts3 <- mun_codes[mun_codes$name == city_name, ]$nuts3

    bkg_conc <- mean(pm_25_urban_ita[pm_25_urban_ita$nuts3 == nuts3, ]$bkg_conc,
                     na.rm = TRUE)

  }

# obtaining average speeds of passive and active mobility modes

  passive_mode <- scenario$mode_from[1]

  active_mode <- scenario$mode_to[1]

  speeds <- NULL

  for (i in c(passive_mode, active_mode)) {

    speeds[[i]] <- comm_matrix_cities_km[comm_matrix_cities_km$mean_of_transp == i, ]$speed_kmh[1]

  }

  # deriving normal commuting days for both passive (ex ante) and active (ex post) commuting modes

  for (i in speeds) {

    day_type

  }


}
