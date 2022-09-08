#' Computing variables of interest to assess the health impact associated with increased physical activity
#'
#' It is an internal function, used by `active_mob_impact`.
#'
#' @keywords internal
#' @param data A data frame obtained with `scenario_builder`.
#' @param met_phy_act A numeric value. The physical effort associated with moderate physical activity (expressed in Mets).
#'     This parameter is used quantify the overall weekly physical activity of new active mobility adopters
#'     performing sports in their free-time.
#' @param rr_phy A numeric value. Relative risk reduction associated with the active mobility mode assesed
#' @param mode_to A string. The active mode to which the shift occurs.
#' @param exp_level_base A numeric value. The minimum level of weekly physical activity (expressed in METs) above
#'     which an individual experiences health benefits.
#' @param max_met_walk A numeric value. The level of physical activity exerted walking above which no additional
#'     health benefits can be obtained.
#' @param max_met_bike A numeric value. The level of physical activity exerted cycling above which no additional
#'     health benefits can be obtained.
#' @param max_met_ebike A numeric value. The level of physical activity exerted e-cycling above which no additional
#'     health benefits can be obtained.
#' @return A data frame similar to the input data frame but including variables of interest to compute the health impact
#'     increased physical activity
phy_imp <- function (data, met_phy_act, max_met_walk, rr_phy,
                     max_met_bike, max_met_ebike, mode_to, exp_level_base) {

  # calculating the actual MET levels, taking into account pre-existing physical activity levels and

  out <- transform(data,
                   commuting_w_met_h = met * weekly_travel_time)

  out$total_weekly_met_h <- out$weekly_phy_act_h * met_phy_act + out$commuting_w_met_h # total weekly physical activity

  # computing the maximum amount of MET/week that have an effect of health

  out <- transform(out, capped_weekly_met = dplyr::case_when( # adjusting the met/week according to the maximum levels that have an effect on health
    mode_to == "walk" & total_weekly_met_h <= max_met_walk ~ total_weekly_met_h,
    mode_to == "walk" & total_weekly_met_h > max_met_walk ~ max_met_walk,
    mode_to %in% c("bike", "ebike") & total_weekly_met_h <= max_met_bike ~ total_weekly_met_h,
    TRUE ~ max_met_bike
  )
  )

  # Calculating the reduced mortality risk rate after the shift

  out$reduced_death_rate_phy <- ifelse(out$capped_weekly_met < exp_level_base, 0,
    (1 - rr_phy) * out$capped_weekly_met / exp_level_base) # relative risk

  out


}
