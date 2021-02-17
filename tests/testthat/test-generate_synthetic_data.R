test_that("Generated synthetic data length is correct.", {
  s.data <- GenSynData()
  expect_equal(nrow(s.data), 1000)
  expect_equal(length(s.data), 8)
  expect_true(is.data.frame(s.data))
})

test_that("Generated synthetic data type is correct.", {
  s.data <- GenSynData()
  expect_true(is.data.frame(s.data))
})

test_that("Arguments provided for Generated synthetic data
           type is correct.", {

  expect_error(GenSynData(sample.size = -10),
               regexp = "'sample_size' should be a positive ineteger numer." )
})


test_that(("Generated synthetic data is as expected."),{

  data_1 <- GenSynData(sample.size = 200, seed=13221, outcome.sd = 20)
  data_2 <- GenSynData(sample.size = 1000, seed=734, gps.spec = 2)
  data_3 <- GenSynData(sample.size = 1250, seed=986, outcome.sd = 8)
  data_4 <- GenSynData(sample.size = 111, seed=1021, gps.spec = 7)

  expect_equal(data_1$cf1[10] , -0.4354315, tolerance = 0.0001)
  expect_equal(data_1$cf5[79] , 0, tolerance = 0.0001)
  expect_equal(data_2$cf3[900], 0.5132489, tolerance = 0.0001)
  expect_equal(data_2$Y[121], -6.741239, tolerance = 0.0001)
  expect_equal(data_3$treat[73], 18.31448, tolerance = 0.0001)
  expect_equal(data_3$cf4[11], 0.8901834, tolerance = 0.0001)
  expect_equal(data_4$cf1[112], data_4$cf1[113])
  expect_equal(data_4$cf2[14], 0.7849783, tolerance = 0.0001)

})
