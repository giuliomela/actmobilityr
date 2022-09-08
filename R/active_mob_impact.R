active_mob_impact <- function (scenario, met_phy_act = 7, rr_phy_bike = 0.899,
                               rr_phy_walk = 0.883, rr_phy_ebike = 0.899,
                               exp_level_base = 11.25, max_met_walk = 31,
                               max_met_bike = 51, max_met_ebike = 51,
                               rr_pm25 = 1.08,
                               voly = 70000, vsl = 3600000, voly_vls_ref_yr = 2016,
                               air_pollution = TRUE, injuries = TRUE,
                               detail = FALSE) {

  #### GENERAL PARAMETERS

  mode_to <- scenario$mode_to[1]

  mode_from <- scenario$mode_from[1]

  ref_yr <- min(scenario$year) # reference year of the analysis (last available year)

  scenario_yr <- max(scenario$year) # end year of the scenario

  rr_phy <- dplyr::case_when( # relative risk of the active mode considered
    mode_to == "walk" ~ rr_phy_walk,
    mode_to == "bike" ~ rr_phy_bike,
    mode_to == "ebike" ~ rr_phy_ebike
  )

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

  speeds <- NULL

  for (i in c(mode_from, mode_to)) {

    speeds[[i]] <- comm_matrix_cities_km[comm_matrix_cities_km$mean_of_transp == i, ]$speed_kmh[1]

  }

  #### ADDING PHYSICAL ACTIVITY DATA

  data <- merge(scenario, phy_act) # adding physical activity data to the scenario generated with scenario_builder

  data <- data[order(data$age, data$year, data$daily_km), ]

  data$individuals <- data$individuals * data$phy_act_share

  # defining daily hours of physical activity

  data <- transform(data, weekly_phy_act_h = dplyr::case_when(
    duration == "MN0" ~ 0,
    duration == "MN1-149" ~ 150 / 2 / 60,
    duration == "MN150-299" ~ 150 / 60,
    TRUE ~ 300 / 60
  )
  )

  #### ADDING MORTALITY DATA

  # adding mortality data, depending on the NUTS3 in which the city is located

  demo <- subset(demo_data, geo == nuts3) # demo data referring to the scenario city

  data <- merge(data, demo[, c("age", "death_rate", "life_exp")])

  #### CREATING AN UPTAKE VARIABLE

  # creating a DF with the uptake by year (health effects are incremental)

  uptake_by_year <- data.frame(year = c(ref_yr:scenario_yr),
                               uptake_rr = c(seq(0.2, 1, 0.2), rep(1, 5)))

  data <- merge(data, uptake_by_year)

  #### ADDING VARIABLES USEFUL TO COMPUTE THE IMPACT OF INCREASED PHYSICAL ACTIVITY

  data_phy <- phy_imp(data, met_phy_act, max_met_walk, rr_phy,
                  max_met_bike, max_met_ebike, mode_to, exp_level_base)

  if (isTRUE(air_pollution)) {

  #### ADDING VARIABLES USEFUL TO COMPUTE THE IMPACT OF INCREASE AIR POLLUTANT INTAKE

  data_pollution <- pollution_imp(data, rr_pm25, nuts3, bkg_conc, speeds, mode_from, mode_to)

  #### JOINING DATASETS AND COMPUTING OVERALL RR CHANGE

  data <- merge(data_phy, data_pollution)

  data$death_rate_decrease <- data$reduced_death_rate_phy - data$increased_death_rate_pol

  } else {

  names(data)[names(data) == "reduced_death_rate_phy"] <- "death_rate_decrease"

  }

  #### COMPUTING AVOIDED/ADDITIONAL DEATHS

  # Computing baseline deaths

  data_l <- split(data, ~ age + duration + daily_km)

  data_l <- lapply(data_l, function(x) within(x, alive_baseline <- pop_evolution(individuals[1], death_rate)))

  data_l <- lapply(data_l, function(x) within(x, baseline_deaths <- ifelse(year == ref_yr,
                                                                           individuals[1] - alive_baseline,
                                                                           dplyr::lag(alive_baseline) - alive_baseline)))

  data <- Reduce(rbind, data_l)

  # adjusting for uptake period

  data$death_rate_decrease <- data$death_rate_decrease * data$uptake_rr

  # computing avoided deaths (or added) proper

  data$avoided_deaths <- data$baseline_deaths * data$death_rate_decrease

  # computing total km travelled

  data$km_tot <- data$weekly_comm_days * data$daily_km * (data$alive_baseline + data$avoided_deaths) * 52

  if (isTRUE(injuries)) {

  #### TAKING INCREASED INJURY RISK INTO ACCOUNT

  data <- dplyr::left_join(data, fat_risk, by = c("mode_to" = "mode"))

  data$additional_deaths_injury <- data$km_tot * data$fat_rate

  # computing avoided deaths net of increased injury risk

  data$avoided_deaths <- data$avoided_deaths - data$additional_deaths_injury

  }

  #### ECONOMIC VALUATION

  # performing benefit transfer for the voly and the vsl

  bt_fct <- btransfer::bt_transfer(policy_site = "Italy", study_yr = voly_vls_ref_yr,
                                   policy_yr = ref_yr)

  adj_voly_vsl <- c(voly, vsl) * bt_fct$bt_fct

  names(adj_voly_vsl) <- c("voly", "vsl")

  # computing the social discount rate

  sdr <- btransfer::compute_sdr("Italy", policy_yr = ref_yr,
                                h = 20)$sdr

  # defining the output data.frame with the monetary evaluation

  data$disc_fct <- (1 + sdr)^(data$year - ref_yr)

  data <- transform(data, avoided_voly = (avoided_deaths * life_exp * adj_voly_vsl["voly"]) / disc_fct,
                    avoided_vsl = (avoided_deaths * adj_voly_vsl["vsl"]) / disc_fct)

  #### DATA CLEANING

  data <- data[, c("year", "city", "age", "mode_from", "mode_to", "duration", "weekly_comm_days",
                  "km_tot",  "avoided_deaths", "avoided_voly", "avoided_vsl")]


  if (isTRUE(detail)) {

    data

  } else {

    data <- dplyr::group_by(data, city, mode_from,
                            mode_to)

    data <- dplyr::summarise(data,
                             avoided_deaths = sum(avoided_deaths),
                             km_tot = sum(km_tot),
                             avoided_voly = sum(avoided_voly),
                             avoided_vsl = sum(avoided_vsl))

    data <- dplyr::ungroup(data)

  }

  # creating final variables

  for (i in c("avoided_voly", "avoided_vsl")) {

    data[[paste0(i, "_km")]] <- data[[i]] / data$km_tot

  }

  data$air_pollution <- air_pollution

  data$injuries <- injuries

  data


}


