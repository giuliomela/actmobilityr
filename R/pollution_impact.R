#' Quantify the (negative) health impact of increased exposure to air pollutants linked to
#' active mobility with respect to passive mobility
#'
#' The function takes as a main input a mobility-change scenario defined with [scenario_builder()].
#' Other parameters all have a default value but can be modified by the user.
#' Only effects on mortality are considered.
#'
#' @inheritParams phy_act_impact
#' @param rr_25 A numeric value. The relative risk (RR) of death associated with a 10 ug/m3 incrase in background
#'     PM2.5 concentrations. The default value of `1.08` is taken from Chen & Hoek (increase in mortality
#'     risk due to a 10 ug/m3 increase in PM2.5)
#' @return a tibble with the outcome of the economic evaluation: additional deaths and their economic value estimated
#'     with both the VOLY and the VSL. Values per km travelled are provided if `detail` is set to `FALSE`.
#' @examples
#' mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)
#'
#' scenario_builder("Palermo", "private_car_driver", "bike", mode_change_distance = mode_change_shares, comm_days = 4, max_km = 10)
#'
#' phy_act_impact(scenario)
#' pollution_impact(scenario)
pollution_impact <- function (scenario, rr_25 = 1.08, voly = 70000, vsl = 3600000, voly_vls_ref_yr = 2016,
                              detail = FALSE) {

# identifying background concentrations of the city considered

  # Identifying city name from scenario data

  city_name <- scenario$city[1]

  nuts3 <- mun_codes[mun_codes$name == city_name, ]$nuts3

  if (city_name %in% pm_25_urban_ita$name) {

    # if the municipality is in the WHO database, the actual value is used

    bkg_conc <- pm_25_urban_ita[pm_25_urban_ita$name == city_name, ]$bkg_conc

  } else {

    # otherwise, the average background concentration of all cities in the province to which
    # the municipality considered belongs is used.



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

  # Adding physical activity data (to derive "normal commuting days")

  data <- merge(scenario, phy_act) # adding physical activity data to the scenario generated with scenario_builder

  data <- data[order(data$age, data$year, data$daily_km), ]

  data$individuals <- data$individuals * data$phy_act_share

  # building a list with "normal" commuting days before and after the modal shift (paddove vs active mode)

  data_norm_comm <- NULL

  for (i in c(passive_mode, active_mode)) {

    mode <- ifelse(i == passive_mode, "mode_from", "mode_to")

    speed <- speeds[[i]]

    data_norm_comm[[i]] <- data[, c("city", "year", "age",
                                    "individuals", "daily_km", mode, "duration")]

    data_norm_comm[[i]] <- transform(data_norm_comm[[i]],
                                     speed = speed,
                                     sleep = 8,
                                     phy = dplyr::case_when( # defining daily hours of physical activity
                                       data_norm_comm[[i]]$duration == "MN0" ~ 0,
                                       data_norm_comm[[i]]$duration == "MN1-149" ~ 150 / 2 / 60 / 7,
                                       data_norm_comm[[i]]$duration == "MN150-299" ~ (300 + 150) / 2 / 60 / 7,
                                       TRUE ~ 300 / 60 / 7
                                     ),
                                     comm = daily_km / speed # defining commuting time
                                     )

    data_norm_comm[[i]] <- transform(data_norm_comm[[i]],
                                     rest = 24 - sleep - phy - comm,
                                     bkg_conc = bkg_conc) # adding background concentrations

    names(data_norm_comm[[i]])[names(data_norm_comm[[i]]) == 'comm'] <- ifelse(
      i == passive_mode, passive_mode, active_mode
    )

    # computing inhaled doses

    data_norm_comm[[i]] <- tidyr::pivot_longer(data_norm_comm[[i]],
                                               sleep:rest,
                                               names_to = "activity",
                                               values_to = "hours")

    data_norm_comm[[i]] <- merge(data_norm_comm[[i]], ventilation_data)

    data_norm_comm[[i]] <- transform(data_norm_comm[[i]],
                                     inhaled_doses = vent_rates * bkg_conc * hours * con_fct)

    data_norm_comm[[i]] <- dplyr::group_by(data_norm_comm[[i]], city, year, age, individuals,
                                           daily_km, duration)

    data_norm_comm[[i]] <- dplyr::summarise(data_norm_comm[[i]],
                                            inhaled_doses = sum(inhaled_doses))

    data_norm_comm[[i]] <- dplyr::ungroup(data_norm_comm[[i]])


    names(data_norm_comm[[i]])[names(data_norm_comm[[i]]) == 'inhaled_doses'] <- paste0('inhaled_doses_', mode)

  }


 # calculating equivalent change and relative risk

  data <- Reduce(merge, data_norm_comm) # merging data frames (inhaled doses before and after the change)

  data$eq_change <- ((data$inhaled_doses_mode_to / data$inhaled_doses_mode_from) - 1) * bkg_conc

  data$rr <- exp(log(rr_25) * data$eq_change / 10)

  # creating data frame with uptake data (full effect of mode change after 5 years)
  uptake_by_year <- data.frame(year = c(min(scenario$year):max(scenario$year)),
                           uptake_rr = c(seq(0.2, 1, 0.2), rep(1, 5)))

  # adding data on mortality and uptake

  data <- Reduce(merge, list(data, demo_data[demo_data$geo == nuts3, c("age", "death_rate", "life_exp")], uptake_by_year))

  data$rr <- 1 + (data$rr - 1) * data$uptake_rr # adjusting RR according to the uptake time

  data$mort_increase <- (data$rr - 1) / data$rr

  # calculating deaths before and after the modal shift

  data_l <- split(data, ~ age + duration + daily_km)

  data_l <- lapply(data_l, function(x) within(x, alive_baseline <- pop_evolution(individuals[1], death_rate)))

  data_l <- lapply(data_l, function(x) within(x, baseline_deaths <- ifelse(year == min(scenario$year),
                                                                          individuals[1] - alive_baseline,
                                                                          dplyr::lag(alive_baseline) - alive_baseline)))
  data <- Reduce(rbind, data_l)

  data$active_deaths <- data$baseline_deaths * data$mort_increase

  # Evaluating additional deaths in economic terms

  data$km_year <- data$alive_baseline * scenario$weekly_comm_days[1] * data$daily_km * 52

  # performing benefit transfer for the voly and the vsl

  bt_fct <- btransfer::bt_transfer(policy_site = "Italy", study_yr = voly_vls_ref_yr,
                                   policy_yr = min(scenario$year))

  adj_voly_vsl <- c(voly, vsl) * bt_fct$bt_fct

  names(adj_voly_vsl) <- c("voly", "vsl")

  # computing the social discount rate

  sdr <- btransfer::compute_sdr("Italy", policy_yr = min(scenario$year),
                                h = 20)$sdr

  # defining the output data.frame with the monetary evaluation

  data$disc_fct <- (1 + sdr)^(data$year - min(scenario$year))

  # defining the economic value of health benefits and preparing final output

  data <- transform(data, additional_voly = (active_deaths * life_exp * adj_voly_vsl["voly"]) / disc_fct,
                    additional_vsl = (active_deaths * adj_voly_vsl["vsl"]) / disc_fct)

  # retaining variables of interest only

  data <- data[, c("year","age", "city", "daily_km", "km_year", "duration",
                   "active_deaths", "additional_voly", "additional_vsl")]

  data$mode_from <- passive_mode

  data$mode_to <- active_mode

  if (isTRUE(detail)) {

    tidyr::as_tibble(data)

  } else {

    data <- tidyr::tibble(city = data$city[1], mode_from = data$mode_from[1], mode_to = data$mode_to[1],
                          km_total = sum(data$km_year), active_deaths = sum(data$active_deaths),
                          additional_voly = sum(data$additional_voly), additional_vsl = sum(data$additional_vsl))

    for (i in paste0("additional_", c("voly", "vsl"))) {

      data[[paste0(i, "_km")]] <- data[[i]] / data[["km_total"]]

    }

    data

  }

}


