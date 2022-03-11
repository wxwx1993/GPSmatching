test_that("generate_synthetic_data works as expected.", {

  # length is correct
  s_data <- generate_syn_data()
  expect_equal(nrow(s_data), 1000)
  expect_equal(length(s_data), 8)
  expect_true(is.data.frame(s_data))

  # Datatype is correct
  s_data <- generate_syn_data()
  expect_true(is.data.frame(s_data))

  # Provided argument is correct
  expect_error(generate_syn_data(sample_size = -10),
               regexp = "'sample_size' should be a positive ineteger numer." )
  expect_error(generate_syn_data(gps_spec = 10))
  expect_error(generate_syn_data(cova_spec = 20))
  expect_error(generate_syn_data(cova_spec = "a number"))

  # generated synthetic data is correct
  set.seed(13221)
  data_1 <- generate_syn_data(sample_size = 200, outcome_sd = 20)
  data_2 <- generate_syn_data(sample_size = 1000, gps_spec = 2)
  data_4 <- generate_syn_data(sample_size = 111, gps_spec = 7)
  data_3 <- generate_syn_data(sample_size = 1250, outcome_sd = 8)
  data_5 <- generate_syn_data(sample_size = 111, gps_spec = 3)
  data_6 <- generate_syn_data(sample_size = 111, gps_spec = 4)
  data_7 <- generate_syn_data(sample_size = 111, gps_spec = 5)
  data_8 <- generate_syn_data(sample_size = 111, gps_spec = 6)
  data_9 <- generate_syn_data(sample_size = 200, gps_spec = 6, cova_spec = 2)

  expect_equal(data_1$cf1[10] , -0.4354315, tolerance = 0.0001)
  expect_equal(data_1$cf5[79] , 0, tolerance = 0.0001)
  expect_equal(data_2$cf3[900], -0.7703616, tolerance = 0.0001)
  expect_equal(data_2$Y[121], -56.27454, tolerance = 0.0001)
  expect_equal(data_3$treat[73], 8.480214, tolerance = 0.0001)
  expect_equal(data_3$cf4[11], 0.1851389, tolerance = 0.0001)
  expect_equal(data_4$cf1[112], data_4$cf1[113])
  expect_equal(data_4$cf2[14],-0.682732, tolerance = 0.0001)
  expect_equal(data_5$cf2[14], 0.1738286, tolerance = 0.0001)
  expect_equal(data_6$cf1[2], -1.989927, tolerance = 0.0001)
  expect_equal(data_7$cf3[28], 0.5890692, tolerance = 0.0001)
  expect_equal(data_8$cf2[70], -1.339188, tolerance = 0.0001)
  expect_equal(data_9$cf5[19], -2, tolerance = 0.0001)
})
