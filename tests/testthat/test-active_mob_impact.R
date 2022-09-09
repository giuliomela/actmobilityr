testthat::test_that("active_mob_impact works", {

  mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)

  cities <- c("Milano", "Roma", "Palermo", "")

  scenario_builder("Palermo", "private_car_driver", "bike",
                   mode_change_distance = mode_change_shares, comm_days = 4, max_km = 10)

  output <- active_mob_impact(scenario)

  testthat::expect_true(tibble::is_tibble(output))

  out_vars <- c("avoided_deaths", "km_tot", "avoided_voly",
                "avoided_vsl", "avoided_voly_km", "avoided_vsl_km") # output variables to test

  out_check <- sapply(out_vars, function(x) is.numeric(output[[x]]))

  for(i in out_check) {

    testthat::expect_true(i)

  } # checking that all output values in the output tibble are numeric



})
