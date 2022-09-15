test_that("pop_evolution works", {

  res_pop <- pop_evolution(500, seq(from = 0.001, to = 0.0006, by = - 0.0001))

  expect_true(is.vector(res_pop)) # result is a vector

  expect_true(is.numeric(res_pop)) # result is a numeric vector

  expect_gt(res_pop[1], res_pop[length(res_pop)]) # first element greater than last

})
