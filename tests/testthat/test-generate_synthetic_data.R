test_that("Generated synthetic data length is correct.", {
  s_data <- generate_syn_data()
  expect_equal(nrow(s_data), 1000)
  expect_equal(length(s_data), 8)
  expect_true(is.data.frame(s_data))
})

test_that("Generated synthetic data type is correct.", {
  s_data <- generate_syn_data()
  expect_true(is.data.frame(s_data))
})

test_that("Arguments provided for Generated synthetic data
           type is correct.", {

  expect_error(generate_syn_data(sample_size = -10),
               regexp = "'sample_size' should be a positive ineteger numer." )
  expect_error(generate_syn_data(gps_spec = 10))
  expect_error(generate_syn_data(cova_spec = 20))
  expect_error(generate_syn_data(cova_spec = "a number"))
})


test_that(("Generated synthetic data is as expected."),{

  data_1 <- generate_syn_data(sample_size = 200, seed=13221, outcome_sd = 20)
  data_2 <- generate_syn_data(sample_size = 1000, seed=734, gps_spec = 2)
  data_3 <- generate_syn_data(sample_size = 1250, seed=986, outcome_sd = 8)
  data_4 <- generate_syn_data(sample_size = 111, seed=1021, gps_spec = 7)

  expect_equal(data_1$cf1[10] , -0.4354315, tolerance = 0.0001)
  expect_equal(data_1$cf5[79] , 0, tolerance = 0.0001)
  expect_equal(data_2$cf3[900], 0.5132489, tolerance = 0.0001)
  expect_equal(data_2$Y[121], -6.741239, tolerance = 0.0001)
  expect_equal(data_3$treat[73], 18.31448, tolerance = 0.0001)
  expect_equal(data_3$cf4[11], 0.8901834, tolerance = 0.0001)
  expect_equal(data_4$cf1[112], data_4$cf1[113])
  expect_equal(data_4$cf2[14], 0.7849783, tolerance = 0.0001)

})
