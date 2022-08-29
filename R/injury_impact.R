injury_impact <- function (tot_distance, mode) {

  # Computing total distances covered during the scenario length

   fat_rate <- fat_rate[fata_rate$mode == mode, ]$fat_rate

   addtional_deaths <- fat_rate * tot_distance

   addtional_deaths

}
