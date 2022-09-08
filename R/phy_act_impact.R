#' Quantifies the health benefits associated with increased physical activity linked to
#' active mobility and compute their economic value.
#'
#' The function takes as a main input a mobility-change scenario defined with [scenario_builder()].
#' Other parameters all have a default value but can be modified by the user.
#' Only effects on mortality are considered.
#'
#' @param scenario A `data.frame` describing a mobility-change scenario defined with [scenario_builder()].
#' @param met_phy_act A numeric value. The physical effort associated with moderate physical activity (expressed in Mets).
#'     This parameter is used quantify the overall weekly physical activity of new active mobility adopters
#'     performing sports in their free-time. Default is `7` MET.
#' @param rr_0_bike A numeric value. Relative risk reduction associated with cycling. The default value is `0.10` and is taken from
#'     the HEAT manual.
#' @param rr_0_walk A numeric value. Relative risk reduction associated with walking. The default value is `0.11` and is taken from
#'     the HEAT manual.
#' @param rr_0_ebike A numeric value. Relative risk reduction associated with riding an e-bike and is assumed to be the same of that
#'     of bikes. The default value is `0.10`.
#' @param exp_level_base A numeric value. The minimum level of weekly physical activity (expressed in METs) above
#'     which an individual experiences health benefits. The default value is `11.25`.
#' @param max_met_walk A numeric value. The level of physical activity exerted walking above which no additional
#'     health benefits can be obtained. The default value, from the HEAT manual, is `31`.
#' @param max_met_bike A numeric value. The level of physical activity exerted cycling above which no additional
#'     health benefits can be obtained. The default value, from the HEAT manual, is `51`.
#' @param max_met_ebike A numeric value. The level of physical activity exerted riding an e-bike above which no additional
#'     health benefits can be obtained. The default value is `51`, assumed equal to that of bikes.
#' @param base_yr A logical value. The reference year of the analysis. The year to which all economic values refer.
#'     The default value is `FALSE`, which means that the reference year is taken from the `scenario` dataframe.
#' @param voly A numeric value. The Value of a Life Year to be used in the economic valuation. The
#'     default value is `70000` euro 2016 et refers to the EU, taken from the 2019 Handbook of the external cost of transport.
#' @param vsl A numeric value. The value of a statistical life, to be used in the economic valuation. The
#'     default value is `3.6` million euro (2016) and refers to the EU. Taken from the 2019 Handbook of the external cost of transport.
#' @param voly_vls_ref_yr A numeric value. The year to which the `voly` and the `vsl` refer. Default is `2016`.
#' @param detail A logical value. If set to `TRUE` a detailed version of the output is provided, with specific
#'     estimates for each age group, physical activity level and year. Default is set to `FALSE`, which return
#'     aggregate values over the whole length of the scenario.
#' @return a tibble with the outcome of the economic evaluation: avoided deaths and their economic value estimated
#'     with both the VOLY and the VSL. Values per km travelled are provided if `detail` is set to `FALSE`.
#' @examples
#' mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)
#'
#' scenario_builder("Palermo", "private_car_driver", "bike", mode_change_distance = mode_change_shares, comm_days = 4, max_km = 10)
#'
#' phy_act_impact(scenario)
#' phy_act_impact(scenario, met_phy_act = 6, detail = TRUE)
phy_act_impact <- function(scenario, met_phy_act = 7, rr_0_bike = 0.1,
                           rr_0_walk = 0.11, rr_0_ebike = 0.1,
                           exp_level_base = 11.25, max_met_walk = 31,
                           max_met_bike = 51, max_met_ebike = 51,
                           base_yr = FALSE, voly = 70000, vsl = 3600000, voly_vls_ref_yr = 2016,
                           detail = FALSE) {

  # identifying the parameters needed for the analysis

  mode_to <- unique(scenario$mode_to)

  if (isFALSE(base_yr)) {

    ref_yr <- min(scenario$year)

  } else {

    ref_yr <- base_yr

  }

  start_yr <- min(scenario$year) # start year of the scenario. Might not coincide with ref_yr

  scenario_yr <- max(scenario$year) # end year of the scenario

  rr <- dplyr::case_when( # relative risk of the active mode considered
    mode_to == "walk" ~ rr_0_walk,
    mode_to == "bike" ~ rr_0_walk,
    mode_to == "ebike" ~ rr_0_ebike
  )

  data <- merge(scenario, phy_act) # adding physical activity data to the scenario generated with scenario_builder

  data <- data[order(data$age, data$year, data$daily_km), ]

  data <- transform(data, individuals = individuals * phy_act_share,
                    commuting_w_met_h = met * weekly_travel_time)

  # calculating the actual MET levels, taking into account pre-existing physical activity levels and

  data <- transform(data, total_weekly_met_h = dplyr::case_when(
    duration == "MN0" ~ commuting_w_met_h,
    duration == "MN1-149" ~ 150 / 2 / 60 *  met_phy_act + commuting_w_met_h,
    duration == "MN150-299" ~ 150 / 60 * met_phy_act + commuting_w_met_h,
    TRUE ~ 300 / 60 * met_phy_act + commuting_w_met_h
  )
  )

  # computing the maximum amount of MET/week that have an effect of health

  data <- transform(data, capped_weekly_met = dplyr::case_when( # adjusting the met/week according to the maximum levels that have an effect on health
    mode_to == "walk" & total_weekly_met_h <= max_met_walk ~ total_weekly_met_h,
    mode_to == "walk" & total_weekly_met_h > max_met_walk ~ max_met_walk,
    mode_to %in% c("bike", "ebike") & total_weekly_met_h <= max_met_bike ~ total_weekly_met_h,
    TRUE ~ max_met_bike
  )
  )

  # adding mortality data, depending on the NUTS3 in which the city is located

  nuts3 <- subset(mun_codes, name == unique(data$city))$nuts3

  demo <- subset(demo_data, geo == nuts3) # demo data referring to the scenario city

  data <- merge(data, demo[, c("age", "death_rate", "life_exp")])

  # Calculating the prevent fraction

  data$prevent_fraction <- ifelse(data$capped_weekly_met < exp_level_base, 0,
                                  data$capped_weekly_met / exp_level_base * rr)

  # creating a DF with the uptake by year (health effects are incremental)
  uptake_by_year <- data.frame(year = c(start_yr:scenario_yr),
                           uptake_rr = c(seq(0.2, 1, 0.2), rep(1, 5)))

  data <- merge(data, uptake_by_year)

  # calculating adjusted mortality rates

  data <- transform(data, adj_death_rate = ifelse(capped_weekly_met < exp_level_base,
                                                  death_rate,
                                                  death_rate - prevent_fraction * uptake_rr * death_rate),
                    adj_prevent_fraction = ifelse(capped_weekly_met < exp_level_base, 0,
                                                  prevent_fraction * uptake_rr))

  data <- data[order(data$age, data$daily_km, data$year), ]

  # calculating avoided deaths

  data_l <- split(data, ~ age + duration + daily_km)

  data_l <- lapply(data_l, function(x) within(x, alive_baseline <- pop_evolution(individuals[1], death_rate)))

  data_l <- lapply(data_l, function(x) within(x, baseline_deaths <- ifelse(year == start_yr,
                                                                          individuals[1] - alive_baseline,
                                                                          dplyr::lag(alive_baseline) - alive_baseline)))
  data <- do.call("rbind", data_l)

  # computing avoided deaths

  data$avoided_deaths <- data$baseline_deaths * data$adj_prevent_fraction

  # Calculating yearly distances

  data <- transform(data, km_year = weekly_comm_days * daily_km * (alive_baseline + avoided_deaths) * 52)

  rownames(data) <- NULL

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

  # defining the economic value of health benefits and preparing final output

  data <- transform(data, avoided_voly = (avoided_deaths * life_exp * adj_voly_vsl["voly"]) / disc_fct,
                    avoided_vsl = (avoided_deaths * adj_voly_vsl["vsl"]) / disc_fct)

  # retaining variables of interest only

  data <- data[, c("year","age", "city", "mode_from", "mode_to", "daily_km", "km_year", "duration",
           "avoided_deaths", "avoided_voly", "avoided_vsl")]

  if (isTRUE(detail)) {

    tidyr::as_tibble(data)

  } else {

    data <- tidyr::tibble(city = data$city[1], mode_from = data$mode_from[1], mode_to = data$mode_to[1],
                          km_total = sum(data$km_year), avoided_deaths = sum(data$avoided_deaths),
                          avoided_voly = sum(data$avoided_voly), avoided_vsl = sum(data$avoided_vsl))

    for (i in paste0("avoided_", c("voly", "vsl"))) {

      data[[paste0(i, "_km")]] <- data[[i]] / data[["km_total"]]

    }

    data

  }

}




