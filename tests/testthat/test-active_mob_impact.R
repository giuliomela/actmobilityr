testthat::test_that("active_mob_impact works", {

  mode_change_shares <- c(0.2, 0.15, 0.1, 0.05)

  cities <- c("Milano", "Roma", "Palermo")

  modes <- c("bike", "ebike", "walk")

  db <- tidyr::expand_grid(city = cities, mode_to = modes)

  scenarios <- dplyr::mutate(db,
                scenarios = purrr::map2(city, mode_to,
                                      function(x, y) scenario_builder(city = x, mode_to = y,
                                      mode_from = "private_car_driver", mode_change_distance = mode_change_shares,
                                           comm_days = 4, max_km = 10))
  )


  scenarios <- dplyr::mutate(scenarios,
    results = lapply(scenarios, function(x) active_mob_impact(x))
  )

  data_to_test <- tidyr::unnest(scenarios[, c("scenarios", "results")], results)

  test_vector <- apply(data_to_test[, 5:10], 2, function(x)is.numeric(x))

  purrr::map(test_vector, function(x) expect_true(x)) # all output columns are numeric

})
