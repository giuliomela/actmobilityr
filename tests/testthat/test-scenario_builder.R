test_that("scenario_builder throws the right errors", {

  # testing that scenario_builder returns an error message when it is expected to

  mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)

  expect_error(scenario_builder("Florence", "private_car_driver",
                                "bike", mode_change_distance = mode_change_shares,
                                comm_days = 4, max_km = 10)) # worng city name

  expect_error(scenario_builder("Firenze", "private_car_driver",
                                c("walk", "bike"), mode_change_distance = mode_change_shares,
                                comm_days = 4, max_km = 10)) # multiple active modes at once

  expect_error(scenario_builder("Firenze", "private_car",
                                "bike", mode_change_distance = mode_change_shares,
                                comm_days = 4, max_km = 10)) # correct passive transportation name

  expect_error(scenario_builder("Firenze", "private_car_driver",
                                "bike", mode_change_distance = mode_change_shares,
                                comm_days = 6, max_km = 10)) # right number of commuting days


})

test_that("scenario_builder works", {

  mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)


  scenario <- scenario_builder("Firenze", "private_car_driver",
                               "bike", mode_change_distance = mode_change_shares,
                               comm_days = 4, max_km = 10)

  expect_true(is.data.frame(scenario))   # output is a data frame


  expect_equal(ncol(scenario), 14) # output data frame has the right number of columns


  #  The vector of individuals shifting to active mobility is made up of numeric values
  expect_true(is.numeric(scenario[scenario$year == min(scenario$year), ]$individuals))

  # Vector of individuals in years different from the base year are NAs

  lapply(scenario[scenario$year != min(scenario$year), ]$individuals,
         function(x) expect_true(is.na(x)))


})
