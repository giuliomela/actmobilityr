pollution_imp <- function(data, rr_pm25, nuts3, bkg_conc, speeds, mode_from, mode_to) {


  # building a list with "normal" commuting days before and after the modal shift (passive vs active mode)

  data_norm_comm <- NULL

  for (i in c(mode_from, mode_to)) {

   # computing daily commuting time

    speed <- speeds[[i]]

    mode <- ifelse(i == mode_from, "mode_from", "mode_to")

    data_norm_comm[[i]] <- transform(data,
                                     speed = speed,
                                     sleep = 8,
                                     phy = weekly_phy_act_h / 7,
                                     comm = daily_km / speed # defining commuting time
    )

    data_norm_comm[[i]] <- transform(data_norm_comm[[i]],
                                     rest = 24 - sleep - phy - comm,
                                     bkg_conc = bkg_conc) # adding background concentrations

    names(data_norm_comm[[i]])[names(data_norm_comm[[i]]) == 'comm'] <- dplyr::case_when(
      i == mode_from & stringr::str_detect(mode_from, "car") ~ "car",
      i == mode_from & stringr::str_detect(mode_from, "motorbike") ~ "mbike",
      i == mode_to ~ mode_to,
      TRUE ~ NA_character_
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

  # calculating equivalent change and relative risk increase after the shift

  out <- Reduce(merge, data_norm_comm) # merging data frames (inhaled doses before and after the change)

  out$eq_change <- ((out$inhaled_doses_mode_to / out$inhaled_doses_mode_from) - 1) * bkg_conc

  out$rr_pollution <- exp(log(rr_pm25) * out$eq_change / 10)

  # computing increase in mortality risk

  out$increased_death_rate_pol <- (out$rr_pollution - 1) / out$rr_pollution

  out

}

